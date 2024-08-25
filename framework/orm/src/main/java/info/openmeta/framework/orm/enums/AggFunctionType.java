package info.openmeta.framework.orm.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import com.google.common.collect.ImmutableMap;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.utils.MapUtils;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Set;

/**
 * Aggregation function types.
 * Supported aggregation functions: sum, avg, max, min, count.
 * Only numeric fields support sum and avg functions.
 * Fields that support max and min functions include: numeric, date, string, option.
 * Any field supports the count function.
 * Note: `COUNT(column_name)` counts only the number of non-null values, and `COUNT(*)` counts all values
 */
@Getter
@AllArgsConstructor
public enum AggFunctionType {
    SUM("sum"), AVG("avg"), MAX("max"), MIN("min"), COUNT("count");

    @JsonValue
    private final String func;

    /**
     * Mapping of aggregation functions to field types
     */
    private static final ImmutableMap<AggFunctionType, Set<FieldType>> FUNCTION_TO_FIELD_TYPE_MAP = MapUtils.<AggFunctionType, Set<FieldType>>builder()
            .put(SUM, FieldType.NUMERIC_TYPES)
            .put(AVG, FieldType.NUMERIC_TYPES)
            .put(MAX, FieldType.COMPARABLE_TYPES)
            .put(MIN, FieldType.COMPARABLE_TYPES)
            .buildImmutableMap();

    /**
     * Get aggregation function type by function name
     *
     * @param type function name
     * @return aggregation function type
     */
    public static AggFunctionType of(String type) {
        Assert.notBlank(type, "Aggregation function name cannot be blank!");
        for (AggFunctionType aggFunctionType : AggFunctionType.values()) {
            if (aggFunctionType.getFunc().equals(type.toLowerCase())) {
                return aggFunctionType;
            }
        }
        throw new IllegalArgumentException("Unsupported aggregate function name: {0}", type);
    }

    /**
     * Determine whether the aggregation function type matches the field type
     *
     * @param functionType      aggregation function type
     * @param fieldType         field type
     * @return true if the aggregation function type matches the field type
     */
    public static boolean validateFunctionType(AggFunctionType functionType, FieldType fieldType) {
        if (COUNT.equals(functionType)) {
            return true;
        }
        Set<FieldType> suitableFieldTypes = FUNCTION_TO_FIELD_TYPE_MAP.get(functionType);
        return suitableFieldTypes != null && suitableFieldTypes.contains(fieldType);
    }
}
