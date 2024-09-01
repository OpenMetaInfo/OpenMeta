package info.openmeta.framework.orm.jdbc.database;

import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.jdbc.database.builder.*;

/**
 * SQL Builder Factory
 */
public class SqlBuilderFactory {

    /**
     * Build SELECT SQL based on model name and FlexQuery object, without pagination:
     *      SELECT mainTableFields, relatedTableFields FROM mainTable
     *      LEFT JOIN relatedTables... ON relations...
     *      WHERE mainTableFilters AND relatedTableFilters...
     *      GROUP BY t.f1,t.f2
     *      ORDER BY t.f1
     *
     * @param modelName Model name
     * @param flexQuery flexQuery
     * @return SELECT SQL
     */
    public static SqlParams buildSelectSql(String modelName, FlexQuery flexQuery) {
        SqlWrapper sqlWrapper = new SqlWrapper(modelName);
        // SELECT query, first process Filter, then build select, groupBy, orderBy clauses in turn.
        SqlBuilderChain chain = new SqlBuilderChain()
                .addBuilder(new WhereBuilder(sqlWrapper, flexQuery))
                .addBuilder(new SelectBuilder(sqlWrapper, flexQuery))
                // Aggregate builder includes groupBy processing, so it must precede the orderBy processing.
                .addBuilder(new AggregateBuilder(sqlWrapper, flexQuery))
                .addBuilder(new OrderByBuilder(sqlWrapper, flexQuery));
        chain.build();
        sqlWrapper.limit(flexQuery.getLimitSize());
        sqlWrapper.buildSelectSql();
        return sqlWrapper.getSqlParams();
    }

    /**
     * Build paging query SQL based on model name and FlexQuery object:
     *      SELECT mainTableFields, relatedTableFields FROM mainTable
     *      LEFT JOIN relatedTables... ON relations...
     *      WHERE mainTableFilters AND relatedTableFilters...
     *      GROUP BY t.f1,t.f2
     *      ORDER BY t.f1
     *      LIMIT 0, 50
     *
     * @param modelName Model name
     * @param flexQuery flexQuery
     * @param page Page object
     * @return SELECT SQL
     */
    public static <T> SqlParams buildPageSql(String modelName, FlexQuery flexQuery, Page<T> page) {
        SqlWrapper sqlWrapper = new SqlWrapper(modelName);
        // SELECT query, first process Filter, then build select, groupBy, orderBy clauses in turn.
        SqlBuilderChain chain = new SqlBuilderChain()
                .addBuilder(new WhereBuilder(sqlWrapper, flexQuery))
                .addBuilder(new SelectBuilder(sqlWrapper, flexQuery))
                .addBuilder(new AggregateBuilder(sqlWrapper, flexQuery))
                // Aggregate builder includes groupBy processing, so it must precede the orderBy processing.
                .addBuilder(new OrderByBuilder(sqlWrapper, flexQuery, page))
                .addBuilder(new PageBuilder(sqlWrapper, flexQuery, page));
        chain.build();
        sqlWrapper.buildSelectSql();
        return sqlWrapper.getSqlParams();
    }

    /**
     * Build Count SQL clause based on model name and FlexQuery object:
     *      SELECT COUNT(*) FROM mainTable
     *      LEFT JOIN relatedTables... ON relations...
     *      WHERE mainTableFilters AND relatedTableFilters
     *
     * @param modelName Model name
     * @param flexQuery Filter conditions
     * @return SELECT SQL
     */
    public static SqlParams buildCountSql(String modelName, FlexQuery flexQuery) {
        SqlWrapper sqlWrapper = new SqlWrapper(modelName);
        // COUNT query, first process Filters object, which is the where clause.
        SqlBuilderChain chain = new SqlBuilderChain()
                .addBuilder(new WhereBuilder(sqlWrapper, flexQuery));
        chain.build();
        sqlWrapper.buildCountSql();
        return sqlWrapper.getSqlParams();
    }

    /**
     * Build TopN SQL clause based on model name and FlexQuery object:
     *      SELECT *
     *      FROM (
     *          SELECT t.*,
     *              ROW_NUMBER() OVER(PARTITION BY t.dept_id ORDER BY t.created_time DESC) as topNumber
     *          FROM emp_info t
     *      ) subQuery
     *      WHERE topNumber <= ?
     *
     * @param modelName Model name
     * @param flexQuery Filter conditions
     * @return SELECT SQL
     */
    public static SqlParams buildTopNSql(String modelName, FlexQuery flexQuery) {
        SqlWrapper sqlWrapper = new SqlWrapper(modelName);
        // SELECT query, first process Filter, then build select, groupBy, orderBy clauses in turn.
        SqlBuilderChain chain = new SqlBuilderChain()
                .addBuilder(new WhereBuilder(sqlWrapper, flexQuery))
                .addBuilder(new SelectBuilder(sqlWrapper, flexQuery))
                .addBuilder(new OrderByBuilder(sqlWrapper, flexQuery));
        chain.build();
        sqlWrapper.buildTopNSql(flexQuery.getGroupBy().getFirst(), flexQuery.getTopN());
        return sqlWrapper.getSqlParams();
    }
}
