package info.openmeta.starter.es.service;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.domain.Page;

/**
 * Common interface for ES service
 * @param <T> entity type stored in ES
 */
public interface ESService<T> {

    /**
     * ES paging query object data
     *
     * @param filters   filter conditions
     * @param orders    sort rules
     * @param page      paging object
     * @param trackTotalHits If the query result does not exceed 10000, it does not need to be set
     * @return a page of indexed data
     */
    Page<T> searchPage(Filters filters, Orders orders, Page<T> page, boolean trackTotalHits);
}