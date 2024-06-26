package info.openmeta.framework.orm.utils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import info.openmeta.framework.base.constant.TimeConstant;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Cast;
import info.openmeta.framework.base.utils.DateUtils;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import jakarta.validation.constraints.NotNull;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.cglib.beans.BeanMap;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Bean utils
 */
@Slf4j
public class BeanTool {

    private BeanTool() {}

    /**
     * Extract the element type of the collection, such as the Object type in List<Object>.
     *
     * @param entityClass entity class
     * @param fieldName field name
     * @return element class
     */
    private static Class<?> getElementClass(Class<?> entityClass, String fieldName) {
        try {
            Field field = entityClass.getDeclaredField(fieldName);
            Type fieldType = field.getGenericType();
            if (fieldType instanceof ParameterizedType parameterizedType) {
                Type[] typeArguments = parameterizedType.getActualTypeArguments();
                if (typeArguments.length > 0 && typeArguments[0] instanceof Class) {
                    return (Class<?>) typeArguments[0];
                }
            }
        } catch (NoSuchFieldException e) {
            throw new IllegalArgumentException(e.getMessage(), e);
        }
        throw new IllegalArgumentException("Cannot extract the element type of the model {0} field {1} in collection.",
                entityClass.getSimpleName(), fieldName);
    }

    /**
     * Convert bean object to map. Null values are not ignored.
     *
     * @param object the bean object
     * @return map
     * @param <T> the bean object type
     */
    public static <T> Map<String, Object> objectToMap(T object) {
        return objectToMap(object, false);
    }

    /**
     * Convert bean object to map.
     *
     * @param object the bean object
     * @param ignoreNull whether to ignore null values
     * @return map
     * @param <T> the bean object type
     */
    public static <T> Map<String, Object> objectToMap(T object, boolean ignoreNull) {
        Map<String, Object> row = new HashMap<>();
        BeanMap beanMap = BeanMap.create(object);
        for (Object key : beanMap.keySet()) {
            if (!(ignoreNull && beanMap.get(key) == null)) {
                row.put((String) key, beanMap.get(key));
            }
        }
        return row;
    }

    /**
     * Object list to Map list. Null values are not ignored.
     *
     * @param objects object list
     * @return Map list
     * @param <T> the object type
     */
    public static <T> List<Map<String, Object>> objectsToMapList(List<T> objects) {
        return objects.stream().map(obj -> objectToMap(obj, false)).collect(Collectors.toList());
    }

    /**
     * Object list to Map list.
     *
     * @param objects object list
     * @param ignoreNull whether to ignore null values
     * @return Map list
     * @param <T> the object type
     */
    public static <T> List<Map<String, Object>> objectsToMapList(List<T> objects, boolean ignoreNull) {
        return objects.stream().map(obj -> objectToMap(obj, ignoreNull)).collect(Collectors.toList());
    }

    /**
     * Object to Map<String, Object>, the object property value will be serialized.
     * @param object object
     * @return Map
     */
    public static Map<String, Object> objectToSerializedMap(Object object) {
        return JsonMapper.getMapper().convertValue(object, new TypeReference<>() {});
    }

    /**
     * Object list to Map list, the object property value will be serialized.
     * @param objects object list
     * @return List<Map>
     */
    public static List<Map<String, Object>> objectsToSerializedMapList(List<?> objects) {
        return objects.stream().map(BeanTool::objectToSerializedMap).collect(Collectors.toList());
    }

    /**
     * Convert string value to object property type, which matched to the fieldType
     * @param fieldTypeClass field type class
     * @param value string value
     * @return object
     */
    private static Object convertStringValue(Class<?> fieldTypeClass, Object value) {
        String stringValue = (String) value;
        if (TimeConstant.DATE_TYPES.contains(fieldTypeClass)) {
            return DateUtils.stringToDateObject(stringValue, fieldTypeClass);
        } else if (List.class.isAssignableFrom(fieldTypeClass)) {
            return StringUtils.isBlank(stringValue) ? FieldType.MULTI_STRING.getDefaultValue() : Arrays.asList(StringUtils.split(stringValue, ","));
        } else if (Enum.class.isAssignableFrom(fieldTypeClass)) {
            // The Enum items are all in uppercase and separated by underscores.
            String enumItem = StringTools.toUpperUnderscoreCase(stringValue);
            return StringUtils.isBlank(stringValue) ? null : Enum.valueOf(Cast.of(fieldTypeClass), enumItem);
        } else if (JsonNode.class.isAssignableFrom(fieldTypeClass)) {
            return StringUtils.isBlank(stringValue) ? null : JsonMapper.stringToObject(stringValue, JsonNode.class);
        } else if (Filters.class.isAssignableFrom(fieldTypeClass)) {
            return Filters.of(stringValue);
        }
        return stringValue;
    }

    /**
     * Convert the original data Map to bean object.
     *
     * @param row original data map
     * @param entityClass bean class
     * @param ignoreNotExist whether to ignore the properties that do not exist in the entity
     * @return bean object
     * @param <T> the bean object type
     */
    public static <T> T originalMapToObject(@NotNull Map<String, Object> row, Class<T> entityClass, boolean ignoreNotExist) {
        T bean;
        try {
            bean = entityClass.getDeclaredConstructor().newInstance();
        } catch (InstantiationException | IllegalAccessException | NoSuchMethodException | InvocationTargetException e) {
            throw new IllegalArgumentException(e.getMessage(), e);
        }
        BeanMap beanMap = BeanMap.create(bean);
        row.forEach((field, value) -> {
            Class<?> fieldTypeClass = beanMap.getPropertyType(field);
            if (fieldTypeClass == null) {
                if (!ignoreNotExist) {
                    log.warn("Entity class {} does not contain attribute {}!", entityClass.getSimpleName(), field);
                }
                return;
            }
            if (value instanceof String) {
                // Convert the string value to the object property type
                value = convertStringValue(fieldTypeClass, value);
            } else if (value instanceof Integer && Long.class.isAssignableFrom(fieldTypeClass)) {
                value = ((Integer) value).longValue();
            } else if (value == null && fieldTypeClass.equals(boolean.class)) {
                value = false;
            }
            try {
                beanMap.put(field, value);
            } catch (ClassCastException e) {
                throw new IllegalArgumentException("Field value type conversion error: {0}", e.getMessage());
            }
        });
        return bean;
    }

    /**
     * Convert the original data Map list to bean object list.
     *
     * @param rows map list of the original data
     * @param entityClass bean class
     * @param ignoreNotExist whether to ignore the properties that do not exist in the entity
     * @return bean object list
     * @param <T> the bean object type
     */
    public static <T> List<T> originalMapListToObjects(List<Map<String, Object>> rows, Class<T> entityClass, boolean ignoreNotExist) {
        return rows.stream().map(row -> originalMapToObject(row, entityClass, ignoreNotExist)).collect(Collectors.toList());
    }

    /**
     * Convert the Map to a bean object.
     * @param row map data
     * @param entityClass bean class
     * @return bean object
     * @param <T> the bean object type
     */
    public static <T> T mapToObject(@NotNull Map<String, Object> row, Class<T> entityClass) {
        T bean;
        try {
            bean = entityClass.getDeclaredConstructor().newInstance();
        } catch (InstantiationException | IllegalAccessException | NoSuchMethodException | InvocationTargetException e) {
            throw new IllegalArgumentException(e.getMessage(), e);
        }
        BeanMap beanMap = BeanMap.create(bean);
        row.forEach((field, value) -> {
            if (value instanceof String) {
                // Convert the string value to the object property type
                Class<?> fieldTypeClass = beanMap.getPropertyType(field);
                if (fieldTypeClass == null) {
                    log.warn("The {} attribute does not exist in the entity class {}!", entityClass.getSimpleName(), field);
                    return;
                } else if (Enum.class.isAssignableFrom(fieldTypeClass)) {
                    value = formatEnumProperty((String) value, fieldTypeClass);
                }
            } else if (value instanceof List && !((List<?>) value).isEmpty()) {
                value = formatListProperty(field, (List<Map<String, Object>>) value, entityClass);
            }
            try {
                beanMap.put(field, value);
            }  catch (ClassCastException e) {
                throw new IllegalArgumentException("Field value type conversion error: {0}", e.getMessage());
            }
        });
        return bean;
    }

    /**
     * Convert string value to Enum property.
     *
     * @param value string value
     * @param fieldTypeClass fieldType class
     * @return Enum
     * @param <T> Enum type
     */
    private static <T extends Enum<T>> T formatEnumProperty(String value, Class<?> fieldTypeClass) {
        // The Enum items are all in uppercase and separated by underscores.
        String enumItem = StringTools.toUpperUnderscoreCase(value);
        return StringUtils.isBlank(value) ? null : Enum.valueOf(Cast.of(fieldTypeClass), enumItem);
    }

    /**
     * Convert List to OneToMany object list.
     *
     * @param field the OneToMany field
     * @param value the OneToMany map data list
     * @param entityClass the entity class
     * @return object list
     * @param <T> the entity class type
     */
    private static <T> List<?> formatListProperty(String field, List<Map<String, Object>> value, Class<T> entityClass) {
        MetaField metaField = ModelManager.getModelField(entityClass.getSimpleName(), field);
        if (FieldType.ONE_TO_MANY.equals(metaField.getFieldType())) {
            // OneToMany field type, need to convert to object list
            Class<?> oneToManyClass = getElementClass(entityClass, field);
            return mapListToObjects(value, oneToManyClass);
        }
        return value;
    }

    /**
     * Convert the Map list to bean object list.
     * @param rows map list
     * @param entityClass bean class
     * @return bean object list
     * @param <T> the bean object type
     */
    public static <T> List<T> mapListToObjects(List<Map<String, Object>> rows, Class<T> entityClass) {
        return rows.stream().map(row -> mapToObject(row, entityClass)).collect(Collectors.toList());
    }

}
