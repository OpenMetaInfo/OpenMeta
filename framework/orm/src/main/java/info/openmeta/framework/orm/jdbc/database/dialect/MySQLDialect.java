package info.openmeta.framework.orm.jdbc.database.dialect;

import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.base.utils.Assert;

import java.util.EnumMap;
import java.util.Map;

/**
 * MySQL database dialect tool class.
 */
public class MySQLDialect implements DialectInterface {
    private static final Map<Operator, String> OPERATOR_MAP = new EnumMap<>(Operator.class);

    static {
        OPERATOR_MAP.put(Operator.EQUAL, "=");
        OPERATOR_MAP.put(Operator.NOT_EQUAL, "!=");
        OPERATOR_MAP.put(Operator.GREATER_THAN, ">");
        OPERATOR_MAP.put(Operator.GREATER_THAN_OR_EQUAL, ">=");
        OPERATOR_MAP.put(Operator.LESS_THAN, "<");
        OPERATOR_MAP.put(Operator.LESS_THAN_OR_EQUAL, "<=");
        OPERATOR_MAP.put(Operator.HAS, "LIKE");
        OPERATOR_MAP.put(Operator.NOT_HAS, "NOT LIKE");
        OPERATOR_MAP.put(Operator.START_WITH, "LIKE");
        OPERATOR_MAP.put(Operator.NOT_START_WITH, "NOT LIKE");
        OPERATOR_MAP.put(Operator.IN, "IN");
        OPERATOR_MAP.put(Operator.NOT_IN, "NOT IN");
        OPERATOR_MAP.put(Operator.BETWEEN, "BETWEEN ? AND ?");
        OPERATOR_MAP.put(Operator.NOT_BETWEEN, "NOT BETWEEN ? AND ?");
        OPERATOR_MAP.put(Operator.IS_SET, "IS NOT NULL");
        OPERATOR_MAP.put(Operator.IS_NOT_SET, "IS NULL");
        OPERATOR_MAP.put(Operator.PARENT_OF, "IN");
        OPERATOR_MAP.put(Operator.CHILD_OF, "LIKE");
    }

    /**
     * Get the predicate of the database query operator: >, =, IN, etc.
     *
     * @param operator FilterUnit operator
     * @return SQL operator predicate string
     */
    public String getPredicate(Operator operator) {
        Assert.isTrue(OPERATOR_MAP.containsKey(operator), """
                Predicate conversion for operator {0} is missing in the MySQL database dialect tool class.
                Check whether the operator is existed in the Operator Enum class.""", operator.getName());
        return OPERATOR_MAP.get(operator);
    }

    /**
     * Get the database paging clause according to the offset and limit.
     *
     * @param offset offset
     * @param limit limit
     * @return paging clause
     */
    public StringBuilder getPageClause(int offset, int limit) {
        return new StringBuilder(" LIMIT ").append(offset).append(",").append(limit);
    }
}
