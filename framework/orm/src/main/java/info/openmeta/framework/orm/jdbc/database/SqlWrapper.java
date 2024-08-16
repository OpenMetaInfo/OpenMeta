package info.openmeta.framework.orm.jdbc.database;

import com.google.common.collect.Sets;
import info.openmeta.framework.base.constant.TimeConstant;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import lombok.Getter;

import java.util.*;

/**
 * SQL Wrapper
 */
public class SqlWrapper {

    public static final String MAIN_TABLE_ALIAS = "t";

    @Getter
    private final String modelName;
    @Getter
    private final String tableName;
    @Getter
    private final SqlParams sqlParams = new SqlParams();

    private final StringBuilder selectClause = new StringBuilder();
    private final StringBuilder joinClause = new StringBuilder();
    private final StringBuilder whereClause = new StringBuilder();
    private final StringBuilder groupByClause = new StringBuilder();
    private final StringBuilder orderByClause = new StringBuilder();
    private StringBuilder limitClause = new StringBuilder();

    /**
     * Model and fields to be accessed, used to check access permissions,
     * check parameter range: fields, filters, orders.
     * Prevent indirect unauthorized access and analysis of data
     * through cascading filtering or cascading sorting.
     */
    private final Map<String, Set<String>> accessModelFields = new HashMap<>();

    /**
     * The associated table alias with the field chain as the key, which may use different fields of the key associated table,
     * but their table aliases are the same table, such as jobId.name, jobId.code;
     * In addition, the associated tables of different field chain keys may be the same table,
     * but are joined through different left join statements, in this case, they are two different table aliases,
     * such as deptId.name, parentDeptId.name
     * For example:
     *      `A left join B on A.f=B.id` only join once when taking multiple fields of B through the field `f`;
     *      `A left join B b1 on A.f1=b1.id left join B b2 on A.f2=b2.id` when A's f1 and f2 fields respectively join B,
     *       it belongs to join multiple times, and table B has different aliases in this case,
     *       so the keys (in chainTableAlias) of B alias is f1, f2 respectively, and the multi-level cascade is the same.
     */
    private final Map<String, String> chainTableAlias = new HashMap<>();

    public SqlWrapper(String modelName) {
        this.modelName = modelName;
        this.tableName = ModelManager.getModel(modelName).getTableName();
    }

    /**
     * SELECT DISTINCT
     */
    public void distinct() {
        selectClause.append(" DISTINCT ");
    }

    /**
     * SELECT COUNT(*)
     */
    public void count() {
        selectClause.append("count(*) AS count").append(",");
    }

    /**
     * Add a column to the select clause
     *
     * @param column column name with alias
     */
    public void select(String column) {
        selectClause.append(column).append(",");
    }

    /**
     * Add columns to the select clause
     *
     * @param columns column names with alias
     */
    public void select(Collection<String> columns) {
        columns.forEach(column -> selectClause.append(column).append(","));
    }

    /**
     * Generate join statement like: `LEFT JOIN table_name t1 ON t1.id = t.job_id`
     *
     * @param metaField left table field object
     * @param leftAlias left table alias
     * @param rightAlias right table alias
     * @param isAcrossTimeline whether to get all timeline slice data
     */
    public void leftJoin(MetaField metaField, String leftAlias, String rightAlias, boolean isAcrossTimeline) {
        joinClause.append(" LEFT JOIN ")
                .append(ModelManager.getModel(metaField.getRelatedModel()).getTableName()).append(" ").append(rightAlias).append(" ")
                .append(" ON ")
                .append(leftAlias).append(".").append(metaField.getColumnName())
                .append(" = ")
                .append(rightAlias).append(".").append(ModelManager.getModelFieldColumn(metaField.getRelatedModel(), metaField.getRelatedField())).append(" ");
        if (ModelManager.isTimelineModel(metaField.getRelatedModel())) {
            // Append `ON` condition when the associated model is a timeline model.
            if (ModelManager.isTimelineModel(metaField.getModelName()) && isAcrossTimeline) {
                // If both the main model and the association model are timeline models, and `isAcrossTimeline = true`,
                // the timeline slice to be associated needs to be determined by adding the effective time condition
                // to the join clause.
                // masterEffectiveOption represents whether the data query of the associated model is based on
                // the start date or end date of the main model data as the effective date of the associated model.
                // Here, it is the end date EFFECTIVE_END_COLUMN of the main model data by default.
                String masterEffectiveOption = leftAlias + "." + ModelConstant.EFFECTIVE_END_COLUMN;
                joinClause.append(" AND ").append(rightAlias).append(".").append(ModelConstant.EFFECTIVE_START_COLUMN).append(" <= ").append(masterEffectiveOption)
                        .append(" AND ").append(rightAlias).append(".").append(ModelConstant.EFFECTIVE_END_COLUMN).append(" >= ").append(masterEffectiveOption);
            } else {
                // When the associated model is a timeline model and specified the `effectiveDate`,
                // add an `ON` condition to the join clause:
                //      AND tn.effectiveStart <= effectiveDate AND tn.effectiveEnd >= effectiveDate
                String effectiveDate = ContextHolder.getContext().getEffectiveDate().format(TimeConstant.DATE_FORMATTER);
                joinClause.append(" AND ").append(rightAlias).append(".").append(ModelConstant.EFFECTIVE_START_COLUMN).append(" <= '").append(effectiveDate).append("' ")
                        .append(" AND ").append(rightAlias).append(".").append(ModelConstant.EFFECTIVE_END_COLUMN).append(" >= '").append(effectiveDate).append("' ");
            }
        }
        if (ModelManager.isMultiTenant(metaField.getRelatedModel())) {
            // Add the `ON` condition of the tenant ID when the associated model is a multi-tenant model:
            //      AND tn.tenantId = tenantId
            joinClause.append(" AND ").append(rightAlias).append(".").append(ModelConstant.TENANT_ID).append(" = ").append(ContextHolder.getContext().getTenantId());
        }
    }

    /**
     * Assign the sql conditions parsed by Filters to the where clause.
     *
     * @param where sql conditions
     */
    public void where(StringBuilder where) {
        whereClause.append(where);
    }

    public void groupBy(List<String> columns) {
        columns.forEach(column -> groupByClause.append(column).append(","));
    }

    public void orderBy(String column, String order) {
        orderByClause.append(column);
        if (Orders.ASC.equals(order)) {
            orderByClause.append(" ASC").append(",");
        } else {
            orderByClause.append(" DESC").append(",");
        }
    }

    /**
     * Limit the number of returned rows in the list query, which cannot be used with paging at the same time.
     *
     * @param limitSize limit size
     */
    public void limit(Integer limitSize) {
        if (limitSize != null) {
            limitClause = new StringBuilder(" LIMIT ").append(limitSize);
        }
    }

    /**
     * Directly assign the paging statement spliced by various database dialect tools
     *
     * @param pageSql sql fragment
     */
    public void page(StringBuilder pageSql) {
        limitClause = pageSql;
    }

    /**
     * Get the alias of the right table by the field chain.
     * If the field chain has been associated with a table alias, return the table alias directly;
     *
     * @param fieldChain Table alias key, that is, the left part of the cascading field,
     *                   such as `jobId` of `jobId.name`, `jobId.typeId` of `jobId.typeId.name`.
     * @return Alias of the right table
     */
    public String getRightTableAliasByFieldChain(String fieldChain) {
        String rightTableAlias;
        if (chainTableAlias.containsKey(fieldChain)) {
            rightTableAlias = chainTableAlias.get(fieldChain);
        } else {
            // Generate a new table alias based on the size +1
            rightTableAlias = MAIN_TABLE_ALIAS + (chainTableAlias.size() + 1);
            // Update the table alias dictionary
            this.chainTableAlias.put(fieldChain, rightTableAlias);
        }
        return rightTableAlias;
    }

    /**
     * Update access model and field
     *
     * @param modelName model name
     * @param fieldName field name
     */
    public void accessModelField(String modelName, String fieldName) {
        if (accessModelFields.containsKey(modelName)) {
            accessModelFields.get(modelName).add(fieldName);
        } else {
            accessModelFields.put(modelName, Sets.newHashSet(fieldName));
        }
    }

    /**
     * Build SELECT SQL:
     *      / * SQL Hint * /
     *      SELECT mainTableFields, relatedTables FROM mainTable
     *      LEFT JOIN relatedTables... ON relations...
     *      WHERE mainTableFilters AND relatedTableFilters...
     *      GROUP BY t.f1,t.f2
     *      ORDER BY t.f1
     *      LIMIT 0,50
     */
    public void buildSelectSql() {
        StringBuilder sql = new StringBuilder("SELECT ").append(StringTools.removeLastComma(selectClause))
                .append(" FROM ").append(tableName).append(" ").append(MAIN_TABLE_ALIAS)
                .append(joinClause);
        if (!whereClause.isEmpty()) {
            sql.append(" WHERE ").append(whereClause);
        }
        if (!groupByClause.isEmpty()) {
            sql.append(" GROUP BY ").append(StringTools.removeLastComma(groupByClause));
        }
        if (!orderByClause.isEmpty()) {
            sql.append(" ORDER BY ").append(StringTools.removeLastComma(orderByClause));
        }
        if (!limitClause.isEmpty()) {
            sql.append(limitClause);
        }
        sqlParams.setSql(DBUtil.wrapHint(sql.toString()));
    }

    /**
     * Build COUNT SQL:
     *      SELECT COUNT(*) FROM mainTable
     *      LEFT JOIN relatedTables... ON relations...
     *      WHERE mainTableFilters AND relatedTableFilters
     */
    public void buildCountSql() {
        StringBuilder sql = new StringBuilder("SELECT COUNT(*) FROM ")
                .append(tableName).append(" ").append(MAIN_TABLE_ALIAS)
                .append(joinClause);
        if (!whereClause.isEmpty()) {
            sql.append(" WHERE ").append(whereClause);
        }
        sqlParams.setSql(DBUtil.wrapHint(sql.toString()));
    }

    /**
     * Add parameterized value, value type: SQLType
     *
     * @param value parameterized value
     */
    public void addArgValue(Object value) {
        sqlParams.addArgValue(value);
    }

}
