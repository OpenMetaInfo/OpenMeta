package info.openmeta.framework.orm.utils;

import com.github.f4b6a3.tsid.TsidCreator;
import com.github.f4b6a3.ulid.UlidCreator;
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
        } else if (id instanceof String) {
            return Long.parseLong((String) id);
        } else {
            return (Long) id;
        }
    }

    /**
     * Convert List<object> to List<Long>.
     *
     * @param objects List<object>
     * @return List<Long>
     */
    private static List<Long> convertIdsToLong(List<?> objects) {
        return objects.stream().map(IdUtils::convertIdToLong).collect(Collectors.toList());
    }

    /**
     * Format ids, when the model primary key field type is Long, convert the id to Long type.
     *
     * @param modelName model name
     * @param ids List<?>
     * @return List<Serializable>
     */
    public static <K extends Serializable> List<K> formatIds(String modelName, String fieldName, List<?> ids) {
        FieldType idType = ModelManager.getModelField(modelName, fieldName).getFieldType();
        if (FieldType.LONG.equals(idType)) {
            return Cast.of(convertIdsToLong(ids));
        } else {
            return Cast.of(ids);
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
    public static <K extends Serializable> List<K> formatIds(String modelName, List<?> ids) {
        return formatIds(modelName, ModelConstant.ID, ids);
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
    public static <K extends Serializable> K formatId(String modelName, String field, Serializable id) {
        if (id == null) {
            return null;
        }
        FieldType fieldType = ModelManager.getModelField(modelName, field).getFieldType();
        if (FieldType.LONG.equals(fieldType)) {
            return Cast.of(convertIdToLong(id));
        } else {
            return Cast.of(id);
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
    public static <K extends Serializable> K formatId(String modelName, Serializable id) {
        return formatId(modelName, ModelConstant.ID, id);
    }

    /**
     * Format id field, when the field type is Long, convert the id to Long type.
     *
     * @param idType id FieldType
     * @param id id
     * @return Object
     */
    public static Object formatId(FieldType idType, Object id) {
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

    /**
     * Generate a 26-character ULID, with 48-bit timestamp and 80-bit random value.
     *
     * @return the ULID id
     */
    public static String getULID() {
        return UlidCreator.getMonotonicUlid().toString();
    }

    /**
     * Generate a Long id, up to 256 nodes and 16,384 unique ids per node per millisecond.
     *
     * @return the Long id
     */
    public static Long getTSIDLong() {
        return TsidCreator.getTsid256().toLong();
    }

    /**
     * Generate a String id, up to 256 nodes and 16,384 unique ids per node per millisecond.
     *
     * @return the String id
     */
    public static String getTSIDString() {
        return TsidCreator.getTsid256().toString();
    }

    /**
     * Get a simple 16-digit id to compatible with the JavaScript Number.MAX_SAFE_INTEGER.
     * Not recommended for use in large concurrent systems.
     *
     * @return Long
     */
    public static Long getSimpleId() {
        return SimpleId.getInstance().nextId();
    }

}
