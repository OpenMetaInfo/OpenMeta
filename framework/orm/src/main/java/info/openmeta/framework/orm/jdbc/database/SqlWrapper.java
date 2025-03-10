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
import org.springframework.util.StringUtils;

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
    @Getter
    private final TableAlias tableAlias = new TableAlias();

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
     * Construct the field segment for the translatable field
     *      COALESCE(NULLIF(trans.column_name, ''), t.column_name) AS column_name
     * Use the original value if the translation field is null or empty.
     *
     * @param metaField translatable field object of the left table
     * @param leftAlias left table alias
     * @param isSelect whether to use in the SELECT statement, if true, add the AS column name
     * @return SQL segment of the translation field
     */
    public String selectTranslatableField(MetaField metaField, String leftAlias, boolean isSelect) {
        String transAlias = tableAlias.getTransTableAlias(leftAlias);
        if (!StringUtils.hasLength(transAlias)) {
            transAlias = tableAlias.generateTransTableAlias(leftAlias);
            this.leftJoinTranslation(metaField, leftAlias, transAlias);
        }
        String columnName = metaField.getColumnName();
        String columnAlias = leftAlias + "." + columnName;
        String selectSegment = "COALESCE(NULLIF(" + transAlias + "." + columnName + ", ''), " + columnAlias + ")";
        if (isSelect) {
            selectSegment += " AS " + columnName;
        }
        return selectSegment;
    }

    /**
     * Replace the field name with the translation field name in the WHERE clause.
     * For example, replace `t.name` with `trans1.name`
     *
     * @param leftAlias left table alias
     * @param metaField translatable field object of the left table
     * @return SQL segment of the translation field to be used in the WHERE clause
     */
    public String filterTranslatableField(String leftAlias, MetaField metaField) {
        String transAlias = tableAlias.getTransTableAlias(leftAlias);
        if (!StringUtils.hasLength(transAlias)) {
            transAlias = tableAlias.generateTransTableAlias(leftAlias);
            this.leftJoinTranslation(metaField, leftAlias, transAlias);
        }
        return transAlias + "." + metaField.getColumnName();
    }

    /**
     * Generate join statement like: `LEFT JOIN table_name t1 ON t.job_id = t1.id`
     *
     * @param metaField left table field object, which is ManyToOne or OneToOne field
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
            this.joinOnTimelineModel(metaField, leftAlias, rightAlias, isAcrossTimeline);
        }
        if (ModelManager.isMultiTenant(metaField.getRelatedModel())) {
            // Add the `ON` condition of the tenant ID when the associated model is a multi-tenant model:
            //      AND tn.tenantId = 'tenantId'
            joinClause.append(" AND ").append(rightAlias).append(".").append(ModelConstant.TENANT_ID)
                    .append(" = ").append("'").append(ContextHolder.getContext().getTenantId()).append("' ");
        }
    }

    /**
     * Generate join statement like:
     *      `LEFT JOIN trans_table_name trans1 ON t1.id = trans1.row_id AND trans1.language_code = ?`
     *
     * @param metaField left table field object
     * @param leftAlias left table alias
     * @param transAlias the alias of the translation table
     */
    public void leftJoinTranslation(MetaField metaField, String leftAlias, String transAlias) {
        String transTableName = ModelManager.getTranslationTableName(metaField.getModelName());
        String currentLanguage = ContextHolder.getContext().getLanguage().getCode();
        joinClause.append(" LEFT JOIN ")
                .append(transTableName).append(" ").append(transAlias).append(" ").append(" ON ")
                .append(leftAlias).append(".").append(ModelConstant.ID).append(" = ")
                .append(transAlias).append(".").append(ModelConstant.TRANS_ROW_ID_COLUMN).append(" ")
                .append(" AND ")
                .append(transAlias).append(".").append(ModelConstant.LANGUAGE_CODE_COLUMN).append(" = '")
                .append(currentLanguage).append("' ");
    }

    /**
     * Append `ON` condition when the associated model is a timeline model.
     *
     * @param metaField left table field object
     * @param leftAlias left table alias
     * @param rightAlias right table alias
     * @param isAcrossTimeline whether to get all timeline slice data
     */
    private void joinOnTimelineModel(MetaField metaField, String leftAlias, String rightAlias, boolean isAcrossTimeline) {
        // Append `ON` condition when the associated model is a timeline model.
        if (ModelManager.isTimelineModel(metaField.getModelName()) && isAcrossTimeline) {
            // If both the main model and the association model are timeline models, and `isAcrossTimeline = true`,
            // the timeline slice to be associated needs to be determined by adding the effective time condition
            // to the join clause.
            // masterEffectiveOption represents whether the data query of the associated model is based on
            // the start date or end date of the main model data as the effective date of the associated model.
            // Here, it is the end date EFFECTIVE_END_COLUMN of the main model data by default.
            String masterEffectiveOption = leftAlias + "." + ModelConstant.EFFECTIVE_END_DATE_COLUMN;
            joinClause.append(" AND ").append(rightAlias).append(".").append(ModelConstant.EFFECTIVE_START_DATE_COLUMN).append(" <= ").append(masterEffectiveOption)
                    .append(" AND ").append(rightAlias).append(".").append(ModelConstant.EFFECTIVE_END_DATE_COLUMN).append(" >= ").append(masterEffectiveOption);
        } else {
            // When the associated model is a timeline model and specified the `effectiveDate`,
            // add an `ON` condition to the join clause:
            //      AND tn.effectiveStartDate <= effectiveDate AND tn.effectiveEndDate >= effectiveDate
            String effectiveDate = ContextHolder.getContext().getEffectiveDate().format(TimeConstant.DATE_FORMATTER);
            joinClause.append(" AND ").append(rightAlias).append(".").append(ModelConstant.EFFECTIVE_START_DATE_COLUMN).append(" <= '").append(effectiveDate).append("' ")
                    .append(" AND ").append(rightAlias).append(".").append(ModelConstant.EFFECTIVE_END_DATE_COLUMN).append(" >= '").append(effectiveDate).append("' ");
        }
    }

    /**
     * Assign the SQL conditions parsed by Filters to the where clause.
     *
     * @param where SQL conditions
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
     * @param pageSql SQL fragment
     */
    public void page(StringBuilder pageSql) {
        limitClause = pageSql;
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
        sqlParams.setSql(sql.toString());
    }

    /**
     * Build TOP N SQL:
     *      SELECT *
     *      FROM (
     *          SELECT t.*,
     *              ROW_NUMBER() OVER(PARTITION BY t.dept_id ORDER BY t.created_time DESC) as topNumber
     *          FROM emp_info t
     *      ) subQuery
     *      WHERE topNumber <= ?
     * <p>
     * Build the `subQuery` first, and then wrap the `subQuery` with the `topNumber` condition.
     *
     * @param partitionColumn partition column, which is the relatedField of OneToMany field
     *
     */
    public void buildTopNSql(String partitionColumn, Integer topN) {
        String windowSql = " ROW_NUMBER() OVER(PARTITION BY " +
                MAIN_TABLE_ALIAS + "." + partitionColumn +
                " ORDER BY " + StringTools.removeLastComma(orderByClause) + ") as topNumber ";
        StringBuilder subSql = new StringBuilder("SELECT ").append(selectClause)
                .append(windowSql)
                .append(" FROM ").append(tableName).append(" ").append(MAIN_TABLE_ALIAS)
                .append(joinClause);
        if (!whereClause.isEmpty()) {
            subSql.append(" WHERE ").append(whereClause);
        }
        String topNSql = "SELECT * FROM (" + subSql + ") subQuery WHERE topNumber <= ?";
        addArgValue(topN);
        sqlParams.setSql(topNSql);
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
        sqlParams.setSql(sql.toString());
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
