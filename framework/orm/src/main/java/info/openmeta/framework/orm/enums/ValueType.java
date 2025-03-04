package info.openmeta.framework.orm.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import info.openmeta.framework.base.utils.Assert;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.math.BigDecimal;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * View type Enum.
 */
@Getter
@AllArgsConstructor
public enum ValueType {
    // String, including long text
    STRING("String", FieldType.STRING, ""),

    // Numeric
    INTEGER("Integer", FieldType.INTEGER, 0),
    LONG("Long", FieldType.LONG, 0L),
    DOUBLE("Double", FieldType.DOUBLE, 0.0),
    BIG_DECIMAL("BigDecimal", FieldType.BIG_DECIMAL, BigDecimal.ZERO),

    // bool
    BOOLEAN("Boolean", FieldType.BOOLEAN, false),

    // time
    DATE("Date", FieldType.DATE, null),
    DATE_TIME("DateTime", FieldType.DATE_TIME, null),
    ;

    @JsonValue
    private final String type;
    private final FieldType fieldType;
    private final Object defaultValue;

    /** type map */
    static private final Map<FieldType, ValueType> TYPE_MAP = Stream.of(values())
            .collect(Collectors.toMap(ValueType::getFieldType, Function.identity()));

    /**
     * Get ValueType by FieldType
     * @param fieldType FieldType
     * @return ValueType
     */
    public static ValueType of(FieldType fieldType) {
        Assert.isTrue(TYPE_MAP.containsKey(fieldType), "FieldType `{0}` not exist in ValueType!", fieldType);
        return TYPE_MAP.get(fieldType);
    }
}
