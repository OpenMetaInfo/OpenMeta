package info.openmeta.framework.orm.jdbc.database.builder;

import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.jdbc.database.SqlWrapper;
import info.openmeta.framework.orm.jdbc.database.DBUtil;

/**
 * Page Builder
 */
public class PageBuilder extends BaseBuilder implements SqlClauseBuilder {

    private final Page<?> page;

    public <T> PageBuilder(SqlWrapper sqlWrapper, FlexQuery flexQuery, Page<T> page) {
        super(sqlWrapper, flexQuery);
        this.page = page;
    }

    /**
     * Different databases might have different paging syntax,
     * implement the getPageClause() method in different database dialect to define the SQL fragment.
     */
    public void build() {
        StringBuilder pageSql = DBUtil.getDialectPageClause(page.getPageNumber(), page.getPageSize());
        sqlWrapper.page(pageSql);
    }
}
