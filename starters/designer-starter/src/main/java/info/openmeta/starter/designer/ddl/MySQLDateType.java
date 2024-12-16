package info.openmeta.starter.designer.ddl;

import info.openmeta.framework.orm.enums.FieldType;

import java.util.EnumMap;
import java.util.Map;

/**
 * Field type mapping for MySQL database.
 * OneToMany, ManyToMany relationship fields do not exist in the database columns.
 * TODO: Maintain the mapping in DesignFieldTypeMapping model.
 */
public abstract class MySQLDateType {
    public static final Map<FieldType, String> FIELD_TYPE_MAP = new EnumMap<>(FieldType.class);

    static {
        FIELD_TYPE_MAP.put(FieldType.STRING, "VARCHAR");
        FIELD_TYPE_MAP.put(FieldType.INTEGER, "INT");
        FIELD_TYPE_MAP.put(FieldType.LONG, "BIGINT");
        FIELD_TYPE_MAP.put(FieldType.DOUBLE, "DECIMAL");
        FIELD_TYPE_MAP.put(FieldType.BIG_DECIMAL, "DECIMAL");
        FIELD_TYPE_MAP.put(FieldType.OPTION, "VARCHAR");
        FIELD_TYPE_MAP.put(FieldType.BOOLEAN, "TINYINT");
        FIELD_TYPE_MAP.put(FieldType.DATE, "DATE");
        FIELD_TYPE_MAP.put(FieldType.DATE_TIME, "DATETIME");
        FIELD_TYPE_MAP.put(FieldType.ONE_TO_ONE, "BIGINT");
        FIELD_TYPE_MAP.put(FieldType.MANY_TO_ONE, "BIGINT");
        // For database readability and portability, use TEXT storage
        FIELD_TYPE_MAP.put(FieldType.JSON, "TEXT");
        FIELD_TYPE_MAP.put(FieldType.MULTI_STRING, "VARCHAR");
        FIELD_TYPE_MAP.put(FieldType.MULTI_OPTION, "VARCHAR");
        FIELD_TYPE_MAP.put(FieldType.FILTERS, "VARCHAR");
        FIELD_TYPE_MAP.put(FieldType.ORDERS, "VARCHAR");
    }

    /**
     * Get dbType according fieldType
     */
    public static String getDbType(FieldType fieldType) {
        return FIELD_TYPE_MAP.get(fieldType);
    }
}
