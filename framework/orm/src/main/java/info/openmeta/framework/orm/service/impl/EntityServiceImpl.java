package info.openmeta.framework.orm.service.impl;

import info.openmeta.framework.base.utils.SFunction;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.domain.SubQueries;
import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.orm.utils.BeanTool;
import info.openmeta.framework.orm.utils.LambdaUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Implementation of the Entity Service.
 * The basic CRUD operations for the entity.
 *
 * @param <T> entity class
 * @param <K> id class
 */
public abstract class EntityServiceImpl<T extends BaseModel, K extends Serializable> implements EntityService<T, K> {

    protected final String modelName;
    private Class<T> entityClass;

    @Autowired
    protected ModelService<K> modelService;

    /** Get the entity class of model. **/
    @SuppressWarnings("unchecked")
    private Class<T> getEntityClass() {
        if (entityClass == null) {
            entityClass = (Class<T>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
        }
        return entityClass;
    }

    /**
     * Get the model name in the constructor, the class name of entity must be equal to the model name.
     */
    protected EntityServiceImpl() {
        this.modelName = getEntityClass().getSimpleName();
    }

    /**
     * Counts the number of rows matching the given filters.
     *
     * @param filters filtering conditions
     * @return the total count of matching rows
     */
    @Override
    public Long count(Filters filters) {
        return modelService.count(modelName, filters);
    }

    /**
     * Creates a new entity and returns its ID.
     *
     * @param entity the entity to be created
     * @return the ID of the newly created entity
     */
    @Override
    public K createOne(T entity) {
        Map<String, Object> rowMap = BeanTool.objectToMap(entity);
        return modelService.createOne(modelName, rowMap);
    }

    /**
     * Creates a new entity and returns the created entity with any auto-generated fields populated.
     *
     * @param entity the entity to be created
     * @return the newly created entity
     */
    @Override
    public T createOneAndFetch(T entity) {
        Map<String, Object> rowMap = BeanTool.objectToMap(entity);
        Map<String, Object> result = modelService.createOneAndFetch(modelName, rowMap, ConvertType.TYPE_CAST);
        return BeanTool.mapToObject(result, entityClass);
    }

    /**
     * Creates multiple entities at once and returns a list of their IDs.
     *
     * @param entities the list of entities to be created
     * @return a list of IDs for the created entities
     */
    @Override
    public List<K> createList(List<T> entities) {
        List<Map<String, Object>> rows = BeanTool.objectsToMapList(entities);
        return modelService.createList(modelName, rows);
    }

    /**
     * Creates multiple entities at once and returns them with any auto-generated fields populated.
     *
     * @param entities the list of entities to be created
     * @return a list of the newly created entities
     */
    @Override
    public List<T> createListAndFetch(List<T> entities) {
        List<Map<String, Object>> rows = BeanTool.objectsToMapList(entities);
        List<Map<String, Object>> results = modelService.createListAndFetch(modelName, rows, ConvertType.TYPE_CAST);
        return BeanTool.mapListToObjects(results, entityClass);
    }

    /**
     * Get an entity by id.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id the id of the entity to get
     * @return an Optional object containing the entity if found, or empty if not found
     */
    @Override
    public Optional<T> getById(K id) {
        return this.getById(id, Collections.emptyList());
    }

    /**
     * Get an entity by id, with subQueries to expand relational fields.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id the id of the entity to get
     * @param subQueries subQueries object, used to expand relational fields
     * @return an Optional object containing the entity if found, or empty if not found
     */
    @Override
    public Optional<T> getById(K id, SubQueries subQueries) {
        Optional<Map<String, Object>> optionalRow = modelService.getById(modelName, id, subQueries);
        return optionalRow.map(row -> BeanTool.mapToObject(row, entityClass));
    }

    /**
     * Get an entity by id, with specified fields to read.
     * If the fields is not specified, all accessible fields as the default.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id the id of the entity to get
     * @param fields field list to read
     * @return an Optional object containing the entity if found, or empty if not found
     */
    @Override
    public Optional<T> getById(K id, Collection<String> fields) {
        Optional<Map<String, Object>> optionalRow = modelService.getById(modelName, id, fields);
        return optionalRow.map(row -> BeanTool.mapToObject(row, entityClass));
    }

    /**
     * Get multiple entities by ids.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param ids a list of ids to get
     * @return a list of entities
     */
    @Override
    public List<T> getByIds(List<K> ids) {
        return this.getByIds(ids, Collections.emptyList());
    }

    /**
     * Get multiple entities by ids.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param ids data ids list
     * @param subQueries subQueries object, can expand relational fields
     * @return a list of entities
     */
    @Override
    public List<T> getByIds(List<K> ids, SubQueries subQueries) {
        List<Map<String, Object>> rows = modelService.getByIds(modelName, ids, Collections.emptyList(), subQueries, ConvertType.TYPE_CAST);
        return CollectionUtils.isEmpty(rows) ? Collections.emptyList() : BeanTool.mapListToObjects(rows, entityClass);
    }

    /**
     * Get multiple entities by ids, with specified fields to read.
     * If the fields is not specified, all accessible fields as the default.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param ids data ids list
     * @param fields field list to read
     * @return a list of entities
     */
    @Override
    public List<T> getByIds(List<K> ids, Collection<String> fields) {
        List<Map<String, Object>> rows = modelService.getByIds(modelName, ids, fields);
        return CollectionUtils.isEmpty(rows) ? Collections.emptyList() : BeanTool.mapListToObjects(rows, entityClass);
    }

    /**
     * Get distinct values for the specified field, filtered by the given conditions.
     *
     * @param <V> the type of the field's value
     * @param fieldReference the field reference to get the value from
     * @param filters optional filtering conditions
     * @return a list of distinct field values
     */
    @Override
    public <V extends Serializable, R> List<V> getDistinctFieldValue(SFunction<T, R> fieldReference, Filters filters) {
        String fieldName = LambdaUtils.getAttributeName(fieldReference);
        return modelService.getDistinctFieldValue(modelName, fieldName, filters);
    }

    /**
     * Get the specified field value by id and field reference.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id data id
     * @param fieldReference field reference to get the value from
     * @return field value
     */
    @Override
    public <V extends Serializable, R> V getFieldValue(K id, SFunction<T, R> fieldReference) {
        String fieldName = LambdaUtils.getAttributeName(fieldReference);
        return modelService.getFieldValue(modelName, id, fieldName);
    }

    /**
     * Get the ids based on the filters.
     *
     * @param filters the filters to apply
     * @return a list of IDs
     */
    @Override
    public List<K> getIds(Filters filters) {
        return modelService.getIds(modelName, filters);
    }

    /**
     * Get the distinct ids for ManyToOne/OneToOne relational field based on the filters.
     *
     * @param <EK> the type of the related entity ID, extending Serializable
     * @param <R> the return type of the method reference
     * @param filters the filters to apply
     * @param fieldReference field method reference to get the value from
     * @return distinct ids for relational field
     */
    @Override
    public <EK extends Serializable, R> List<EK> getRelatedIds(Filters filters, SFunction<T, R> fieldReference) {
        String fieldName = LambdaUtils.getAttributeName(fieldReference);
        return modelService.getRelatedIds(modelName, filters, fieldName);
    }

    /**
     * Get the distinct ids for ManyToOne/OneToOne relational field based on the filters.
     *
     * @param <EK> the type of the related entity ID, extending Serializable
     * @param filters filters
     * @param fieldName relational field name
     * @return distinct ids for relational field
     */
    @Override
    public <EK extends Serializable> List<EK> getRelatedIds(Filters filters, String fieldName) {
        return modelService.getRelatedIds(modelName, filters, fieldName);
    }

    /**
     * Updates an existing entity by its ID. Null values are ignored.
     *
     * @param entity the entity with updated values
     * @return `true` if the update was successful; otherwise, an exception is thrown
     */
    @Override
    public boolean updateOne(T entity) {
        Map<String, Object> rowMap = BeanTool.objectToMap(entity, true);
        return modelService.updateOne(modelName, rowMap);
    }

    /**
     * Updates an existing entity by its ID, with an option to ignore null values.
     *
     * @param entity the entity with updated values
     * @param ignoreNull if `true`, null values are ignored; otherwise, they overwrite existing values
     * @return `true` if the update was successful; otherwise, an exception is thrown
     */
    @Override
    public boolean updateOne(T entity, boolean ignoreNull) {
        Map<String, Object> rowMap = BeanTool.objectToMap(entity, ignoreNull);
        return modelService.updateOne(modelName, rowMap);
    }

    /**
     * Updates an existing entity by its ID. Null values are ignored.
     * Returns the updated entity with the latest field values.
     *
     * @param entity the entity with updated values
     * @return the updated entity with the latest field values
     */
    @Override
    public T updateOneAndFetch(T entity) {
        Map<String, Object> rowMap = BeanTool.objectToMap(entity, true);
        Map<String, Object> result = modelService.updateOneAndFetch(modelName, rowMap, ConvertType.TYPE_CAST);
        return BeanTool.mapToObject(result, entityClass);
    }

    /**
     * Updates an existing entity by its ID, with an option to ignore null values.
     * Returns the updated entity with the latest field values.
     *
     * @param entity the entity with updated values
     * @param ignoreNull if `true`, null values are ignored; otherwise, they overwrite existing values
     * @return the updated entity with the latest field values
     */
    @Override
    public T updateOneAndFetch(T entity, boolean ignoreNull) {
        Map<String, Object> rowMap = BeanTool.objectToMap(entity, ignoreNull);
        Map<String, Object> result = modelService.updateOneAndFetch(modelName, rowMap, ConvertType.TYPE_CAST);
        return BeanTool.mapToObject(result, entityClass);
    }

    /**
     * Updates multiple entities by their IDs. Null values are ignored.
     *
     * @param entities the list of entities to be updated
     * @return `true` if the update was successful; otherwise, an exception is thrown
     */
    @Override
    public boolean updateList(List<T> entities) {
        List<Map<String, Object>> rows = BeanTool.objectsToMapList(entities, true);
        return modelService.updateList(modelName, rows);
    }

    /**
     * Updates multiple entities by their IDs, with an option to ignore null values.
     *
     * @param entities the list of entities to be updated
     * @param ignoreNull if `true`, null values are ignored; otherwise, they overwrite existing values
     * @return `true` if the update was successful; otherwise, an exception is thrown
     */
    @Override
    public boolean updateList(List<T> entities, boolean ignoreNull) {
        List<Map<String, Object>> rows = BeanTool.objectsToMapList(entities, ignoreNull);
        return modelService.updateList(modelName, rows);
    }

    /**
     * Updates multiple entities by their IDs. Null values are ignored.
     * Returns the updated entities with the latest field values.
     *
     * @param entities the list of entities to be updated
     * @return a list of updated entities with the latest field values
     */
    @Override
    public List<T> updateListAndFetch(List<T> entities) {
        List<Map<String, Object>> rows = BeanTool.objectsToMapList(entities, true);
        List<Map<String, Object>> results = modelService.updateListAndFetch(modelName, rows, ConvertType.TYPE_CAST);
        return BeanTool.mapListToObjects(results, entityClass);
    }

    /**
     * Updates multiple entities by their IDs, with an option to ignore null values.
     * Returns the updated entities with the latest field values.
     *
     * @param entities the list of entities to be updated
     * @param ignoreNull if `true`, null values are ignored; otherwise, they overwrite existing values
     * @return a list of updated entities with the latest field values
     */
    @Override
    public List<T> updateListAndFetch(List<T> entities, boolean ignoreNull) {
        List<Map<String, Object>> rows = BeanTool.objectsToMapList(entities, ignoreNull);
        List<Map<String, Object>> results = modelService.updateListAndFetch(modelName, rows, ConvertType.TYPE_CAST);
        return BeanTool.mapListToObjects(results, entityClass);
    }

    /**
     * Performs a batch update of rows that match the provided filters,
     * updating the fields specified in the value map.
     * <p>If no filters are specified, all data visible to the current user might be updated.</p>
     *
     * @param filters optional filter criteria
     * @param value a map of field-value pairs to update
     * @return the number of rows affected
     */
    @Override
    public Integer updateByFilter(Filters filters, Map<String, Object> value) {
        return modelService.updateByFilter(modelName, filters, value);
    }

    /**
     * Deletes an entity by its ID.
     *
     * @param id the ID of the entity to be deleted
     * @return `true` if the deletion was successful; otherwise, an exception is thrown
     */
    @Override
    public boolean deleteById(K id) {
        return modelService.deleteById(modelName, id);
    }

    /**
     * Deletes multiple entities by their IDs.
     *
     * @param ids the list of IDs of the entities to be deleted
     * @return `true` if the deletion was successful; otherwise, an exception is thrown
     */
    @Override
    public boolean deleteByIds(List<K> ids) {
        return modelService.deleteByIds(modelName, ids);
    }

    /**
     * Deletes entities based on the specified filters.
     *
     * @param filters the filters that determine which entities to delete
     * @return `true` if the deletion was successful; otherwise, an exception is thrown
     */
    @Override
    public boolean deleteByFilters(Filters filters) {
        return modelService.deleteByFilters(modelName, filters);
    }

    /**
     * Query one entity that matches the specified filters.
     * If multiple entities match, an exception is thrown.
     *
     * @param filters the filters used to find the entity
     * @return the single matching entity
     */
    @Override
    public T searchOne(Filters filters) {
        return this.searchOne(new FlexQuery(filters));
    }

    /**
     * Query one entity that matches the specified FlexQuery, which can set fields to read.
     * If multiple entities match, an exception is thrown.
     *
     * @param flexQuery FlexQuery object containing fields, filters, orders, etc.
     * @return the single matching entity
     */
    @Override
    public T searchOne(FlexQuery flexQuery) {
        Map<String, Object> row = modelService.searchOne(modelName, flexQuery);
        return CollectionUtils.isEmpty(row) ? null : BeanTool.mapToObject(row, entityClass);
    }

    /**
     * Query all entities without pagination. Only for code use.
     * If the result exceeds the MAX_BATCH_SIZE, an error is logged, but no exception is thrown.
     *
     * @return a list of all entities
     */
    @Override
    public List<T> searchList() {
        return this.searchList(new FlexQuery());
    }

    /**
     * Query all entities that match the specified filters without pagination.
     * If the result exceeds the MAX_BATCH_SIZE, an error is logged, but no exception is thrown.
     *
     * @param filters the filters used to find the entities
     * @return a list of matching entities
     */
    @Override
    public List<T> searchList(Filters filters) {
        return this.searchList(new FlexQuery(filters));
    }

    /**
     * Query all entities that match the specified flexQuery without pagination.
     * If the result exceeds the MAX_BATCH_SIZE, an error log is recorded, but no exception is thrown.
     *
     * @param flexQuery FlexQuery object containing fields, filters, orders, etc.
     * @return a list of matching entities
     */
    @Override
    public List<T> searchList(FlexQuery flexQuery) {
        return modelService.searchList(modelName, flexQuery, entityClass);
    }

    /**
     * Executes a FlexQuery without pagination and maps the results to the specified DTO type.
     * If the result exceeds the MAX_BATCH_SIZE, an error is logged, but no exception is thrown.
     *
     * @param <R> the DTO type
     * @param flexQuery FlexQuery object containing fields, filters, sorting, etc.
     * @param dtoClass the class of the DTO type
     * @return a list of DTO objects of the specified type
     */
    @Override
    public <R> List<R> searchList(FlexQuery flexQuery, Class<R> dtoClass) {
        return modelService.searchList(modelName, flexQuery, dtoClass);
    }

    /**
     * Performs a paginated query based on a FlexQuery.
     * <p>The page size must not exceed MAX_BATCH_SIZE.</p>
     *
     * @param flexQuery a FlexQuery object containing fields, filters, sorting, etc.
     * @param page a Page object containing pagination information
     * @return a Page containing the requested entities
     */
    @Override
    public Page<T> searchPage(FlexQuery flexQuery, Page<T> page) {
        return modelService.searchPage(modelName, flexQuery, page, entityClass);
    }

    /**
     * Performs a paginated query based on a FlexQuery and maps the results to a specified DTO type.
     * <p>The page size must not exceed MAX_BATCH_SIZE.</p>
     *
     * @param <R> the DTO type
     * @param flexQuery a FlexQuery object containing fields, filters, sorting, etc.
     * @param page a Page object containing pagination information
     * @param dtoClass the class of the DTO type
     * @return a Page containing the requested DTO objects
     */
    @Override
    public <R> Page<R> searchPage(FlexQuery flexQuery, Page<R> page, Class<R> dtoClass) {
        return modelService.searchPage(modelName, flexQuery, page, dtoClass);
    }

    /**
     * Groups entities by their IDs based on the provided filters.
     * <p>If the result exceeds MAX_BATCH_SIZE, an error is logged but no exception is thrown.</p>
     *
     * @param filters the filters used to find the entities
     * @return a map of IDs to the corresponding entities
     */
    @Override
    public Map<Serializable, T> groupById(Filters filters) {
        List<T> objects = this.searchList(filters);
        if (!CollectionUtils.isEmpty(objects)) {
            return objects.stream().collect(Collectors.toMap(BaseModel::getId, Function.identity()));
        }
        return Collections.emptyMap();
    }
}
