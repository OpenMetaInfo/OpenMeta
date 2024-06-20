package info.openmeta.framework.orm.utils;

import info.openmeta.framework.orm.enums.FieldType;
import lombok.extern.slf4j.Slf4j;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Number utility class
 */
@Slf4j
public class NumberUtils {


    /**
     * Truncate the specified number of decimal places
     *
     * @param score number
     * @param scale decimal places
     * @return number after truncation
     */
    public static double scale(double score, int scale) {
        BigDecimal b = new BigDecimal(String.valueOf(score));
        return b.setScale(scale, RoundingMode.DOWN).stripTrailingZeros().doubleValue();
    }

    /**
     * Convert number object to long type
     * @param number number object
     * @return long type number
     */
    public static long numberToLong(Object number) {
        if (number == null) {
            return 0L;
        }
        if (number instanceof Number) {
            return ((Number) number).longValue();
        }
        try {
            return Long.parseLong(number.toString());
        } catch (NumberFormatException e) {
            log.error("Convert number {} to long error: {}", number, e.getMessage());
            return 0L;
        }
    }

    /**
     * Add two numbers with specified data type
     * @param v1 number 1
     * @param v2 number 2
     * @param fieldType data type
     * @return sum of two numbers
     */
    private static Object sumNumber(Object v1, Object v2, FieldType fieldType) {
        Object value = null;
        // Compatible with null values
        if (v1 == null) {
            v1 = fieldType.getDefaultValue();
        }
        if (v2 == null) {
            v2 = fieldType.getDefaultValue();
        }
        switch (fieldType) {
            case INTEGER:
                v2 = v2 instanceof BigDecimal ? ((BigDecimal) v2).intValue() : v2;
                value = Integer.sum((Integer) v1, (Integer) v2); break;
            case LONG:
                v2 = v2 instanceof BigDecimal ? ((BigDecimal) v2).longValue() : v2;
                value = Long.sum((Long) v1, (Long) v2); break;
            case DOUBLE:
                v2 = v2 instanceof BigDecimal ? ((BigDecimal) v2).doubleValue() : v2;
                value = Double.sum((Double) v1, (Double) v2); break;
            case BIG_DECIMAL:
                value = ((BigDecimal) v1).add((BigDecimal) v2); break;
            default: break;
        }
        return value;
    }

    /**
     * Summarize the data list according to the specified numeric field name and data type
     *
     * @param rows data list
     * @param numericFieldsType numeric fields and their data types
     * @return Map after summing the numeric fields
     */
    public static Map<String, Object> sumNumericFields(List<Map<String, Object>> rows, Map<String, FieldType> numericFieldsType) {
        Map<String, Object> result = numericFieldsType.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, entry -> entry.getValue().getDefaultValue()));
        for (Map<String, Object> row : rows) {
            numericFieldsType.forEach((field, type) -> result.put(field, sumNumber(result.get(field), row.get(field), type)));
        }
        return result;
    }
}
