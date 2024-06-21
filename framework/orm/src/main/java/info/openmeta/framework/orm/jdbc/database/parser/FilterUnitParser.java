package info.openmeta.framework.orm.jdbc.database.parser;

import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.FilterUnit;
import info.openmeta.framework.orm.jdbc.database.DBUtil;
import info.openmeta.framework.orm.jdbc.database.SqlWrapper;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;

import java.util.*;

/**
 * FilterUnit parser to get the sql condition.
 */
public class FilterUnitParser {

    /**
     * Build the sql fragment based on the calculated field alias(table alias + column name)
     *
     * @param sqlWrapper sql wrapper
     * @param tableAlias table alias of the field of current filterUnit
     * @param metaField field object corresponding to the current filterUnit
     * @param filterUnit filterUnit
     */
    public static StringBuilder parse(SqlWrapper sqlWrapper, String tableAlias, MetaField metaField, FilterUnit filterUnit) {
        StringBuilder fieldAlias = new StringBuilder(tableAlias).append(".").append(metaField.getColumnName());
        StringBuilder condition = new StringBuilder(fieldAlias);
        Operator operator = filterUnit.getOperator();
        Object value = filterUnit.getValue();
        switch (operator) {
            case EQUAL:
            case NOT_EQUAL:
            case GREATER_THAN:
            case GREATER_THAN_OR_EQUAL:
            case LESS_THAN:
            case LESS_THAN_OR_EQUAL:
                condition.append(" ").append(DBUtil.getPredicate(operator));
                if (value instanceof String expression && StringTools.isReservedField((String) value)) {
                    // Handle field comparison, value is @{fieldName}, which is a reserved field name,
                    // embed it into sql after checking the field name is legal
                    String fieldName = expression.substring(2, expression.length() - 1).trim();
                    String columnName = ModelManager.getModelFieldColumn(metaField.getModelName(), fieldName);
                    condition.append(" ").append(tableAlias).append(".").append(columnName).append(" ");
                } else {
                    condition.append(" ?");
                    sqlWrapper.addArgValue(value);
                }
                break;
            case HAS:
            case NOT_HAS:
                condition.append(" ").append(DBUtil.getPredicate(operator)).append(" ?");
                value = "%" + value + "%";
                sqlWrapper.addArgValue("%" + value + "%");
                break;
            case START_WITH:
            case NOT_START_WITH:
                condition.append(" ").append(DBUtil.getPredicate(operator)).append(" ?");
                value = value + "%";
                sqlWrapper.addArgValue(value);
                break;
            case IN:
            case NOT_IN:
                condition.append(" ").append(DBUtil.getPredicate(operator)).append(" (");
                StringBuilder inSql = new StringBuilder();
                ((Collection<?>) value).forEach( v -> {
                    inSql.append("?,");
                    sqlWrapper.addArgValue(v);
                });
                StringTools.removeLastComma(inSql);
                condition.append(inSql).append(")");
                break;
            case BETWEEN:
            case NOT_BETWEEN:
                condition.append(" ").append(DBUtil.getPredicate(operator)).append(" ");
                sqlWrapper.addArgValue(((List<?>) value).get(0));
                sqlWrapper.addArgValue(((List<?>) value).get(1));
                break;
            case IS_SET:
            case IS_NOT_SET:
                condition.append(" ").append(DBUtil.getPredicate(operator));
                break;
            case PARENT_OF:
                condition = parseParentOf(sqlWrapper, tableAlias, operator, value);
                break;
            case CHILD_OF:
                condition = parseChildOf(sqlWrapper, fieldAlias, operator, value);
                break;
            default:
                throw new IllegalArgumentException("FilterUnitParser currently does not support the operator {0}! ", operator.getName());
        }
        return condition;
    }

    /**
     * Parse the PARENT_OF filter condition.
     * When the value of PARENT_OF is a single value, the IN condition is constructed by splitting the idPath
     * separated by "/". which is: id IN split(idPath, "/")
     * <p>
     * When the value of PARENT_OF is a list, the IN condition is constructed by combining the id set extracted from
     * the idPath separated by "/". which is: id IN Set(split(idPath1, "/") + split(idPath2, "/") + split(idPath3, "/"))
     *
     * @param sqlWrapper sql wrapper
     * @param tableAlias table alias
     * @param operator operator
     * @param value value
     * @return sql condition
     */
    private static StringBuilder parseParentOf(SqlWrapper sqlWrapper, String tableAlias, Operator operator, Object value) {
        StringBuilder condition = new StringBuilder(tableAlias).append(".").append(ModelConstant.ID).append(" ").append(DBUtil.getPredicate(operator)).append(" (");
        // Extract the id set from the idPath separated by "/"
        Set<Long> parentIds = new HashSet<>();
        for (Object path : (Collection<?>) value) {
            parentIds.addAll(StringTools.splitIdPath((String) path));
        }
        // Build the IN condition
        StringBuilder inSql = new StringBuilder();
        (parentIds).forEach( v -> {
            inSql.append("?,");
            sqlWrapper.addArgValue(v);
        });
        StringTools.removeLastComma(inSql);
        condition.append(inSql).append(")");
        return condition;
    }

    /**
     * Parse the CHILD_OF filter condition.
     * When the value of CHILD_OF is a single value, the StartWith condition is constructed,
     * which equals to `t.field LIKE ?, value%`, that can use the prefix index.
     * <p>
     * When the value of CHILD_OF is a list, the OR condition is constructed by combining the StartWith condition,
     * which is: `(t.field LIKE ? OR t.field LIKE ?), value1%, value2%`
     *
     * @param sqlWrapper sql wrapper
     * @param fieldAlias field alias
     * @param operator operator
     * @param value value
     * @return sql condition
     */
    private static StringBuilder parseChildOf(SqlWrapper sqlWrapper, StringBuilder fieldAlias, Operator operator, Object value) {
        StringBuilder condition;
        Collection<?> paths = (Collection<?>) value;
        int size = paths.size();
        if (size == 1) {
            return buildStartWithCondition(sqlWrapper, fieldAlias, operator, paths.iterator().next());
        } else if (size > 1) {
            condition = new StringBuilder(" (");
            Iterator<?> iterator = paths.iterator();
            for (int i = 0; i < size; i++) {
                if (i > 0) {
                    condition.append(" OR ");
                }
                condition.append(buildStartWithCondition(sqlWrapper, fieldAlias, operator, iterator.next()));
            }
            condition.append(")");
        } else {
            throw new IllegalArgumentException("The value of [{0}, {1}, {2}] cannot be empty!", fieldAlias, operator, value);
        }
        return condition;
    }

    /**
     * When the value is a single value, use StartWith to construct a simple query condition,
     * which can use the prefix index: `t.f LIKE ?, value%`
     */
    private static StringBuilder buildStartWithCondition(SqlWrapper sqlWrapper, StringBuilder fieldAlias, Operator operator, Object value) {
        StringBuilder condition = new StringBuilder(fieldAlias).append(" ").append(DBUtil.getPredicate(operator)).append(" ?");
        value = value + "%";
        sqlWrapper.addArgValue(value);
        return condition;
    }
}
