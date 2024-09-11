package info.openmeta.framework.orm.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.databind.JsonNode;
import com.google.common.collect.Sets;
import info.openmeta.framework.base.constant.TimeConstant;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.DateUtils;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.Orders;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.math.BigDecimal;
import java.sql.Types;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Field type Enum.
 */
@Slf4j
@Getter
@AllArgsConstructor
public enum FieldType {
    // String, including long text
    STRING("String", "String", "", String.class, Types.VARCHAR),

    // Numeric
    INTEGER("Integer", "Integer", 0, Integer.class, Types.INTEGER),
    LONG("Long", "Long", 0L, Long.class, Types.BIGINT),
    DOUBLE("Double", "Decimal", 0.00, Double.class, Types.DOUBLE),
    BIG_DECIMAL("BigDecimal", "High-Precision Decimal", new BigDecimal("0"), BigDecimal.class, Types.DECIMAL),

    // bool
    BOOLEAN("Boolean", "Yes/No", false, Boolean.class, Types.BOOLEAN),

    // time
    DATE("Date", "Date", null, LocalDate.class, Types.DATE),
    DATE_TIME("DateTime", "DateTime", null, LocalDateTime.class, Types.TIMESTAMP),

    // OptionList, MultiOption, MultiString, JSON, Filter
    OPTION("Option", "Single Option", "", String.class, Types.VARCHAR),
    MULTI_OPTION("MultiOption", "Multi-Option", new ArrayList<>(0), List.class, Types.VARCHAR),
    MULTI_STRING("MultiString", "Multi-String", new ArrayList<>(0), List.class, Types.VARCHAR),
    JSON("JSON", "JSON", null, JsonNode.class, Types.LONGVARCHAR),
    FILTERS("Filters", "Filter", null, Filters.class, Types.VARCHAR),
    ORDERS("Orders", "Orders", null, Orders.class, Types.VARCHAR),

    // Relational fields
    ONE_TO_ONE("OneToOne", "OneToOne", null, null, Types.BIGINT),
    MANY_TO_ONE("ManyToOne", "ManyToOne", null, null, Types.BIGINT),
    ONE_TO_MANY("OneToMany", "OneToMany", new ArrayList<>(0), null, Types.NULL),
    MANY_TO_MANY("ManyToMany", "ManyToMany", new ArrayList<>(0), null, Types.NULL);

    @JsonValue
    private final String type;
    private final String name;
    // The default value when database value is null.
    private final Object defaultValue;
    private final Class<?> javaType;
    private final int sqlType;

    // Relational field type set
    public static final Set<FieldType> RELATED_TYPES = Sets.immutableEnumSet(ONE_TO_ONE, MANY_TO_ONE, ONE_TO_MANY, MANY_TO_MANY);
    // OneToOne, ManyToOne
    public static final Set<FieldType> TO_ONE_TYPES = Sets.immutableEnumSet(ONE_TO_ONE, MANY_TO_ONE);
    // OneToMany, ManyToMany
    public static final Set<FieldType> TO_MANY_TYPES = Sets.immutableEnumSet(ONE_TO_MANY, MANY_TO_MANY);
    // Numeric field type set
    public static final Set<FieldType> NUMERIC_TYPES = Sets.immutableEnumSet(INTEGER, LONG, DOUBLE, BIG_DECIMAL);
    // Comparable field type set
    public static final Set<FieldType> COMPARABLE_TYPES = Sets.immutableEnumSet(INTEGER, LONG, DOUBLE, BIG_DECIMAL, DATE, DATE_TIME, STRING, OPTION);
    // Expandable field type set
    public static final Set<FieldType> EXPANDABLE_TYPES = Sets.immutableEnumSet(BOOLEAN, MANY_TO_ONE, ONE_TO_ONE, OPTION, MULTI_OPTION);

    /**
     * type map
     */
    private static final Map<String, FieldType> typeMap = Stream.of(values()).collect(Collectors.toMap(FieldType::getType, Function.identity()));

    /**
     * Get field type by string type
     *
     * @param type string
     * @return FieldType
     */
    public static FieldType of(String type) {
        Assert.notBlank(type, "Field type cannot be blank!");
        return typeMap.get(type);
    }

    /**
     * Convert string value to object value according to field type
     *
     * @param fieldType field type
     * @param value     string value
     * @return object value of field type
     */
    public static Object convertStringToObject(FieldType fieldType, String value) {
        Object result;
        if (StringUtils.isBlank(value)) {
            result = fieldType.getDefaultValue();
        } else {
            try {
                value = truncateDefaultValue(value);
                // ManyToOne/OneToOne is processed as Long.
                result = switch (fieldType) {
                    case INTEGER -> Integer.valueOf(value);
                    case LONG, MANY_TO_ONE, ONE_TO_ONE -> Long.valueOf(value);
                    case DOUBLE -> Double.valueOf(value);
                    case BIG_DECIMAL -> new BigDecimal(value);
                    case BOOLEAN -> value.equals("1") || StringUtils.lowerCase(value).equals("true");
                    case DATE ->
                            // The `now` parameter (for the current time) is instantiated when the value is taken.
                            TimeConstant.NOW.equalsIgnoreCase(value) ? null : DateUtils.stringToDateObject(value, LocalDate.class);
                    case DATE_TIME ->
                            TimeConstant.NOW.equalsIgnoreCase(value) ? null : DateUtils.stringToDateObject(value, LocalDateTime.class);
                    // Other cases are processed as strings.
                    default -> value;
                };
            } catch (Exception e) {
                throw new IllegalArgumentException("field.default.value.incorrect.{0}.{1}", fieldType.getType(), value, e);
            }
        }
        return result;
    }

    /**
     * Remove the `'` and `"` at the beginning and end of the default value string.
     * <p>
     * This method could be removed when the default value does not have this flag bit.
     *
     * @param value default value string
     * @return string after removing ' and " at the beginning and end
     */
    private static String truncateDefaultValue(String value) {
        if (value.startsWith("'") || value.startsWith("\"")) {
            value = value.substring(1);
        }
        if (value.endsWith("'") || value.endsWith("\"")) {
            value = value.substring(0, value.length() - 1);
        }
        return value;
    }
}
