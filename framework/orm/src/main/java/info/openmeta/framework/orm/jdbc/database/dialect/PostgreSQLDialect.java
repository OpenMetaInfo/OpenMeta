package info.openmeta.framework.orm.jdbc.database.dialect;

import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.base.utils.Assert;

import java.util.EnumMap;
import java.util.Map;

/**
 * PostgreSQL Dialect
 */
public abstract class PostgreSQLDialect {
    private static final Map<Operator, String> OPERATOR_MAP = new EnumMap<>(Operator.class);

    static {
        OPERATOR_MAP.put(Operator.EQUAL, "=");
        OPERATOR_MAP.put(Operator.NOT_EQUAL, "!=");
        OPERATOR_MAP.put(Operator.GREATER_THAN, ">");
        OPERATOR_MAP.put(Operator.GREATER_THAN_OR_EQUAL, ">=");
        OPERATOR_MAP.put(Operator.LESS_THAN, "<");
        OPERATOR_MAP.put(Operator.LESS_THAN_OR_EQUAL, "<=");
        OPERATOR_MAP.put(Operator.HAS, "ILIKE");
        OPERATOR_MAP.put(Operator.NOT_HAS, "NOT ILIKE");
        OPERATOR_MAP.put(Operator.START_WITH, "ILIKE");
        OPERATOR_MAP.put(Operator.NOT_START_WITH, "NOT ILIKE");
        OPERATOR_MAP.put(Operator.IN, "IN");
        OPERATOR_MAP.put(Operator.NOT_IN, "NOT IN");
        OPERATOR_MAP.put(Operator.BETWEEN, "BETWEEN ? AND ?");
        OPERATOR_MAP.put(Operator.NOT_BETWEEN, "NOT BETWEEN ? AND ?");
        OPERATOR_MAP.put(Operator.IS_SET, "IS NOT NULL");
        OPERATOR_MAP.put(Operator.IS_NOT_SET, "IS NULL");
        OPERATOR_MAP.put(Operator.PARENT_OF, "IN");
        OPERATOR_MAP.put(Operator.CHILD_OF, "ILIKE");
    }

    /**
     * Convert Operator to sql predicate
     * @param operator Operator
     * @return sql predicate string
     */
    public static String getPredicate(Operator operator) {
        Assert.isTrue(OPERATOR_MAP.containsKey(operator),
                "{0} predicate not supported in PostgreSQL dialect", operator.getName());
        return OPERATOR_MAP.get(operator);
    }
}
