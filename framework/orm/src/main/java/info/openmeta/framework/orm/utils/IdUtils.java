package info.openmeta.framework.orm.utils;

import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Cast;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * ID primary field format, mainly used for an id field type is Long,
 * the generic interface parameter may be String or Integer, need to convert the id parameter to Long type.
 */
public class IdUtils {

    private IdUtils() {}

    /**
     * Convert object id to Long.
     *
     * @param id object
     * @return Long
     */
    public static Long convertIdToLong(Object id) {
        if (id instanceof Integer) {
            return ((Integer) id).longValue();
        }
        return id instanceof String ? Long.parseLong((String) id) : (Long) id;
    }

    /**
     * Convert List<object> to List<Long>.
     *
     * @param objects List<object>
     * @return List<Long>
     */
    private static <K extends Serializable> List<Long> convertIdsToLong(List<K> objects) {
        return objects.stream().map(IdUtils::convertIdToLong).collect(Collectors.toList());
    }

    /**
     * Convert List<?> to List<Serializable>.
     *
     * @param ids List<?>
     * @return List<Serializable>
     */
    public static List<Serializable> typeCastIds(List<?> ids, String modelName, String fieldName) {
        FieldType idType = ModelManager.getModelField(modelName, fieldName).getFieldType();
        List<Serializable> castedIds;
        try {
            castedIds = Cast.of(ids);
            castedIds = IdUtils.formatIds(castedIds, idType);
        } catch (Exception e) {
            throw new IllegalArgumentException("Failed to cast {0}:{1} value to List<Serializable>: {2}",
                    modelName, fieldName, ids, e);
        }
        return castedIds;
    }

    /**
     * Format ids, when the field type is Long, convert the id to Long type.
     *
     * @param ids List<Serializable>
     * @param idType id FieldType
     * @return List<K>
     */
    public static <K extends Serializable> List<K> formatIds(List<K> ids, FieldType idType) {
        if (FieldType.LONG.equals(idType)) {
            return Cast.of(convertIdsToLong(ids));
        } else {
            return ids;
        }
    }

    /**
     * Format ids, when the model primary key field type is Long, convert the id to Long type.
     *
     * @param modelName model name
     * @param ids List<Serializable>
     * @return List<K>
     * @param <K> K
     */
    public static <K extends Serializable> List<K> formatIds(String modelName, List<K> ids) {
        FieldType idType = ModelManager.getModelField(modelName, ModelConstant.ID).getFieldType();
        if (FieldType.LONG.equals(idType)) {
            return Cast.of(convertIdsToLong(ids));
        } else {
            return ids;
        }
    }

    /**
     * Format single id, when the model primary key field type is Long, convert the id to Long type.
     *
     * @param modelName model name
     * @param field field name
     * @param id id
     * @return K
     */
    public static Serializable formatId(String modelName, String field, Object id) {
        if (id == null) {
            return null;
        } else if (id instanceof Serializable) {
            return formatId(modelName, field, (Serializable) id);
        } else {
            throw new IllegalArgumentException("Unaccepted id type: {0} for model-field {1}:{2}", id, modelName, field);
        }
    }

    /**
     * Format single id, when the model primary key field type is Long, convert the id to Long type.
     *
     * @param modelName model name
     * @param field field name
     * @param id id
     * @return K
     * @param <K> K
     */
    public static <K extends Serializable> K formatId(String modelName, String field, K id) {
        if (id == null) {
            return null;
        }
        FieldType fieldType = ModelManager.getModelField(modelName, field).getFieldType();
        if (FieldType.LONG.equals(fieldType)) {
            return Cast.of(convertIdToLong(id));
        } else {
            return id;
        }
    }

    /**
     * Format single id, when the model primary key field type is Long, convert the id to Long type.
     *
     * @param modelName model name
     * @param id id
     * @return K
     * @param <K> K
     */
    public static <K extends Serializable> K formatId(String modelName, K id) {
        return formatId(modelName, ModelConstant.ID, id);
    }

    /**
     * Format id field, when the field type is Long, convert the id to Long type.
     *
     * @param id id
     * @param idType id FieldType
     * @return Object
     */
    public static Object formatId(Object id, FieldType idType) {
        if (FieldType.LONG.equals(idType)) {
            return Cast.of(convertIdToLong(id));
        } else {
            return id;
        }
    }

    /**
     * Format the primary key field in the Map.
     * When the model primary key field type is Long, convert the id to Long type.
     *
     * @param modelName model name
     * @param row Map<String, Object>
     */
    public static void formatMapId(String modelName, Map<String, Object> row) {
        MetaField pkField = ModelManager.getModelPrimaryKeyField(modelName);
        String pk = pkField.getFieldName();
        if (FieldType.LONG.equals(pkField.getFieldType())) {
            row.put(pk, convertIdToLong(row.get(pk)));
        }
    }

    /**
     * Format the primary key field in the List<Map>.
     * When the model primary key field type is Long, convert the id to Long type.
     *
     * @param modelName model name
     * @param rows List<Map<String, Object>>
     */
    public static void formatMapIds(String modelName, List<Map<String, Object>> rows) {
        MetaField pkField = ModelManager.getModelPrimaryKeyField(modelName);
        String pk = pkField.getFieldName();
        if (FieldType.LONG.equals(pkField.getFieldType())) {
            rows.forEach(row -> row.put(pk, convertIdToLong(row.get(pk))));
        }
    }

    /**
     * Determine if the id is a valid value.
     *
     * @param id id value
     * @return boolean
     */
    public static boolean validId(Serializable id) {
        return id != null && !id.equals(0) && !id.equals(0L) && !id.equals("");
    }
}
