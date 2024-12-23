package info.openmeta.starter.es.service.impl;

import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.FilterUnit;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.enums.FilterType;
import info.openmeta.framework.orm.enums.LogicOperator;
import info.openmeta.starter.es.service.ESService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.mapping.IndexCoordinates;
import org.springframework.data.elasticsearch.core.query.Criteria;
import org.springframework.data.elasticsearch.core.query.CriteriaQuery;
import org.springframework.data.elasticsearch.core.query.Query;

import java.lang.reflect.ParameterizedType;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Implementation of Common ES service
 * @param <T> entity type stored in ES
 */
public abstract class ESServiceImpl<T> implements ESService<T> {

    private Class<T> entityClass;

    @Autowired
    private ElasticsearchOperations esOperations;

    /** Get entity class **/
    @SuppressWarnings("unchecked")
    private Class<T> getEntityClass() {
        if (entityClass == null) {
            entityClass = (Class<T>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
        }
        return entityClass;
    }

    protected ESServiceImpl() {
        this.entityClass = getEntityClass();
    }

    /**
     * ES service implementation class must specify the index name by implementing this method
     *
     * @return index name
     */
    public abstract String getIndexName();

    /**
     * ES paging query object data
     *
     * @param filters   filter conditions
     * @param orders    sort rules
     * @param page      paging object
     * @param trackTotalHits If the query result does not exceed 10000, it does not need to be set
     * @return a page of indexed data
     */
    public Page<T> searchPage(Filters filters, Orders orders, Page<T> page, boolean trackTotalHits) {
        Criteria criteria = this.convertFilters(filters);
        Query query = new CriteriaQuery(criteria);
        // ES offset is started from 0, so the calculation is '(page - 1) * pageSize'
        Pageable pageable = PageRequest.of(page.getPageNumber() - 1, page.getPageSize());
        query.setPageable(pageable);
        // Sort by multiple fields
        if (orders != null) {
            orders.getOrderList().forEach(order -> {
                String field = order.get(0);
                String direction = order.get(1);
                Sort.Direction sortDirection = Orders.DESC.equals(direction) ? Sort.Direction.DESC : Sort.Direction.ASC;
                query.addSort(Sort.by(sortDirection, field));
            });
        }
        // Set trackTotalHits
        query.setTrackTotalHits(trackTotalHits);
        // Execute query
        SearchHits<T> searchHits = esOperations.search(query, getEntityClass(), IndexCoordinates.of(getIndexName()));
        List<T> entityList = searchHits.getSearchHits().stream().map(SearchHit::getContent).toList();
        page.setTotal(searchHits.getTotalHits());
        page.setRows(entityList);
        return page;
    }

    /**
     * Convert Filters to ES query criteria
     *
     * @param filters Filters object
     * @return ES query criteria
     */
    private Criteria convertFilters(Filters filters) {
        Criteria criteria = new Criteria();
        if (FilterType.TREE.equals(filters.getType())) {
            if (LogicOperator.AND.equals(filters.getLogicOperator())) {
                filters.getChildren().forEach(child -> criteria.and(convertFilters(child)));
            } else if (LogicOperator.OR.equals(filters.getLogicOperator())) {
                filters.getChildren().forEach(child -> criteria.or(convertFilters(child)));
            }
        } else if (FilterType.LEAF.equals(filters.getType())) {
            criteria.and(convertFilterUnit(filters.getFilterUnit()));
        }
        return criteria;
    }

    /**
     * Convert FilterUnit to ES query criteria
     *
     * @param filterUnit FilterUnit
     * @return ES query criteria
     */
    private Criteria convertFilterUnit(FilterUnit filterUnit) {
        // TODO: validate field metadata existence for ES object
        String field = filterUnit.getField();
        Object value = filterUnit.getValue();
        Criteria criteria = new Criteria(field);
        return switch (filterUnit.getOperator()) {
            case EQUAL -> criteria.is(value);
            case NOT_EQUAL -> criteria.not().is(value);
            case GREATER_THAN -> criteria.greaterThan(value);
            case GREATER_THAN_OR_EQUAL -> criteria.greaterThanEqual(value);
            case LESS_THAN -> criteria.lessThan(value);
            case LESS_THAN_OR_EQUAL -> criteria.lessThanEqual(value);
            case CONTAINS -> criteria.contains(value.toString());
            case NOT_CONTAINS -> criteria.not().contains(value.toString());
            case START_WITH -> criteria.startsWith(value.toString());
            case NOT_START_WITH -> criteria.not().startsWith(value.toString());
            case IN -> criteria.in((Collection<?>) value);
            case NOT_IN -> criteria.not().in((Collection<?>) value);
            case BETWEEN -> {
                List<?> values = (List<?>) value;
                yield criteria.between(values.get(0), values.get(1));
            }
            case NOT_BETWEEN -> {
                List<?> notValues = (List<?>) value;
                yield criteria.not().between(notValues.get(0), notValues.get(1));
            }
            case IS_SET -> criteria.exists();
            case IS_NOT_SET -> criteria.not().exists();
            case PARENT_OF -> buildParentOf(filterUnit);
            case CHILD_OF -> buildChildOf(filterUnit);
        };
    }

    /**
     * Build ES parent query condition
     * Extract the id collection from the idPath separated by "/", such as "1/2/3" -> [1, 2, 3]
     * @param filterUnit FilterUnit
     * @return ES parent query condition
     */
    private Criteria buildParentOf(FilterUnit filterUnit) {
        Set<Long> parentIds = new HashSet<>();
        for (Object path : (Collection<?>) filterUnit.getValue()) {
            parentIds.addAll(StringTools.splitIdPath((String) path));
        }
        return new Criteria(ModelConstant.ID).in(parentIds);
    }

    /**
     * Build ES child query condition
     * @param filterUnit FilterUnit
     * @return ES child query condition
     */
    private Criteria buildChildOf(FilterUnit filterUnit) {
        Criteria criteria = new Criteria();
        for (Object path : (Collection<?>) filterUnit.getValue()) {
            criteria = criteria.or(new Criteria(filterUnit.getField()).startsWith((String) path));
        }
        return criteria;
    }
}