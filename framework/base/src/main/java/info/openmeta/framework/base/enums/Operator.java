package info.openmeta.framework.base.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import com.google.common.collect.Sets;
import info.openmeta.framework.base.utils.Assert;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * FilterUnit Operator
 */
@Getter
@AllArgsConstructor
public enum Operator {
    EQUAL("=", "eq"),
    NOT_EQUAL("!=", "ne"),
    GREATER_THAN(">", "gt"),
    GREATER_THAN_OR_EQUAL(">=", "ge"),
    LESS_THAN("<", "lt"),
    LESS_THAN_OR_EQUAL("<=", "le"),

    CONTAINS("CONTAINS", "contains"),
    NOT_CONTAINS("NOT CONTAINS", "notContains"),

    START_WITH("START WITH", "startWith"),
    NOT_START_WITH("NOT START WITH", "notStartWith"),

    IN("IN", "in"),
    NOT_IN("NOT IN", "notIn"),

    BETWEEN("BETWEEN", "between"),
    NOT_BETWEEN("NOT BETWEEN", "notBetween"),

    /** equals to `not null` */
    IS_SET("IS SET", "isSet"),
    /** equals to `is null` */
    IS_NOT_SET("IS NOT SET", "isNotSet"),

    /** Query multi-level parent nodes, supports multiple values */
    PARENT_OF("PARENT OF", "parentOf"),

    /** Query multi-level child nodes, supports multiple values */
    CHILD_OF("CHILD OF", "childOf");

    @JsonValue
    private final String name;
    private final String abbr;

    /** Supports `@{fieldName}` reserved field name as comparison value */
    public static final Set<Operator> COMPARISON_OPERATORS = Sets.newHashSet(EQUAL, NOT_EQUAL, GREATER_THAN, GREATER_THAN_OR_EQUAL, LESS_THAN, LESS_THAN_OR_EQUAL);
    /** String matching operator */
    public static final Set<Operator> MATCHING_OPERATORS = Sets.newHashSet(CONTAINS, NOT_CONTAINS, START_WITH, NOT_START_WITH);

    public static final Set<Operator> ASSIGNED_OPERATORS = Sets.newHashSet(IS_SET, IS_NOT_SET);

    public static final Set<Operator> COLLECTION_OPERATORS = Sets.newHashSet(IN, NOT_IN, BETWEEN, NOT_BETWEEN, PARENT_OF, CHILD_OF);

    /** On XToMany query conditions, the negative operators are reversed to positive operators */
    public static final Set<Operator> TO_MANY_NEGATIVE_OPERATORS = Sets.newHashSet(NOT_EQUAL, NOT_CONTAINS, NOT_START_WITH, NOT_IN, NOT_BETWEEN);

    /** Names map */
    static private final Map<String, Operator> namesMap = Stream.of(values()).collect(Collectors.toMap(Operator::getName, Function.identity()));

    /**
     * Get operator by name
     */
    public static Operator of(String name) {
        Assert.notBlank(name, "operator.name.cannot.empty");
        Operator operator = namesMap.get(name.toUpperCase());
        Assert.notNull(operator, "operator.{0}.not.exist", name);
        return operator;
    }

    /**
     * Reverse the operator
     */
    public Operator reverse() {
        return switch (this) {
            case NOT_EQUAL -> EQUAL;
            case NOT_CONTAINS -> CONTAINS;
            case NOT_START_WITH -> START_WITH;
            case NOT_IN -> IN;
            case NOT_BETWEEN -> BETWEEN;
            default -> this;
        };
    }

}
