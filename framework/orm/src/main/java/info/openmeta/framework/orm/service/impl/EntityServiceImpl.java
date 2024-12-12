package info.openmeta.framework.orm.service.impl;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.orm.utils.BeanTool;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.lang.reflect.ParameterizedType;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
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
     * Create a single data object and return the id.
     *
     * @param object data object to be created
     * @return id
     */
    @Override
    public K createOne(T object) {
        Map<String, Object> rowMap = BeanTool.objectToMap(object);
        return modelService.createOne(modelName, rowMap);
    }

    /**
     * Create a single data object, return the object with id and other latest field values
     *
     * @param object data object to be created
     * @return object with id and other latest field values.
     */
    @Override
    public T createOneAndReturn(T object) {
        Map<String, Object> rowMap = BeanTool.objectToMap(object);
        Map<String, Object> result = modelService.createOneAndReturn(modelName, rowMap, ConvertType.TYPE_CAST);
        return BeanTool.mapToObject(result, entityClass);
    }

    /**
     * Create multiple data objects and return the id list.
     *
     * @param objects data object list to be created
     * @return id list
     */
    @Override
    public List<K> createList(List<T> objects) {
        List<Map<String, Object>> rows = BeanTool.objectsToMapList(objects);
        return modelService.createList(modelName, rows);
    }

    /**
     * Create multiple data objects, return the object list with id and other latest field values.
     *
     * @param objects data object list to be created
     * @return object list, each object with id and other latest field values.
     */
    @Override
    public List<T> createListAndReturn(List<T> objects) {
        List<Map<String, Object>> rows = BeanTool.objectsToMapList(objects);
        List<Map<String, Object>> results = modelService.createListAndReturn(modelName, rows, ConvertType.TYPE_CAST);
        return BeanTool.mapListToObjects(results, entityClass);
    }

    /**
     * Read one data object by id.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id data id
     * @return data object
     */
    @Override
    public T readOne(K id) {
        return this.readOne(id, Collections.emptyList());
    }

    /**
     * Read one data object by id.
     * If the fields is not specified, all accessible fields as the default.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id data id
     * @param fields field list to read
     * @return data object
     */
    @Override
    public T readOne(K id, Collection<String> fields) {
        Map<String, Object> row = modelService.readOne(modelName, id, fields);
        return CollectionUtils.isEmpty(row) ? null : BeanTool.mapToObject(row, entityClass);
    }

    /**
     * Read multiple data objects by ids.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param ids data ids list
     * @return data object list
     */
    @Override
    public List<T> readList(List<K> ids) {
        return this.readList(ids, null);
    }

    /**
     * Read multiple data objects by ids.
     * If the fields are not specified, all accessible fields as the default.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param ids data ids list
     * @param fields field list to read
     * @return data object list
     */
    @Override
    public List<T> readList(List<K> ids, Collection<String> fields) {
        List<Map<String, Object>> rows = modelService.readList(modelName, ids, null);
        return CollectionUtils.isEmpty(rows) ? Collections.emptyList() : BeanTool.mapListToObjects(rows, entityClass);
    }

    /**
     * Update one data object by its ID. Null values are not ignored.
     *
     * @param object the data object to update
     * @return true / Exception
     */
    @Override
    public boolean updateOne(T object) {
        Map<String, Object> rowMap = BeanTool.objectToMap(object, false);
        return modelService.updateOne(modelName, rowMap);
    }

    /**
     * Update one data object by its ID.
     *
     * @param object the data object to update
     * @param ignoreNull whether to ignore null values during the update
     * @return true / Exception
     */
    @Override
    public boolean updateOne(T object, boolean ignoreNull) {
        Map<String, Object> rowMap = BeanTool.objectToMap(object, ignoreNull);
        return modelService.updateOne(modelName, rowMap);
    }

    /**
     * Update one data object by its ID. Null values are not ignored.
     * Return the updated object fetched from the database with the latest field values.
     *
     * @param object the data object to update
     * @return the updated object fetched from the database with the latest field values.
     */
    @Override
    public T updateOneAndReturn(T object) {
        Map<String, Object> rowMap = BeanTool.objectToMap(object, false);
        Map<String, Object> result = modelService.updateOneAndReturn(modelName, rowMap, ConvertType.TYPE_CAST);
        return BeanTool.mapToObject(result, entityClass);
    }

    /**
     * Update one data object by its ID.
     * Return the updated object fetched from the database with the latest field values.
     *
     * @param object the data object to update
     * @param ignoreNull whether to ignore null values during the update
     * @return the updated object fetched from the database, with the latest field values.
     */
    @Override
    public T updateOneAndReturn(T object, boolean ignoreNull) {
        Map<String, Object> rowMap = BeanTool.objectToMap(object, ignoreNull);
        Map<String, Object> result = modelService.updateOneAndReturn(modelName, rowMap, ConvertType.TYPE_CAST);
        return BeanTool.mapToObject(result, entityClass);
    }

    /**
     * Update multiple data objects by their IDs. Null values are not ignored.
     *
     * @param objects the list of data objects to update
     * @return true / Exception
     */
    @Override
    public boolean updateList(List<T> objects) {
        List<Map<String, Object>> rows = BeanTool.objectsToMapList(objects, false);
        return modelService.updateList(modelName, rows);
    }

    /**
     * Update multiple data objects by their IDs.
     *
     * @param objects the list of data objects to update
     * @param ignoreNull whether to ignore null values during the update
     * @return true / Exception
     */
    @Override
    public boolean updateList(List<T> objects, boolean ignoreNull) {
        List<Map<String, Object>> rows = BeanTool.objectsToMapList(objects, ignoreNull);
        return modelService.updateList(modelName, rows);
    }

    /**
     * Update multiple data objects by their IDs. Null values are not ignored.
     * Return the updated object fetched from the database with the latest field values.
     *
     * @param objects the list of data objects to update
     * @return the updated objects fetched from the database with the latest field values.
     */
    @Override
    public List<T> updateListAndReturn(List<T> objects) {
        List<Map<String, Object>> rows = BeanTool.objectsToMapList(objects, false);
        List<Map<String, Object>> results = modelService.updateListAndReturn(modelName, rows, ConvertType.TYPE_CAST);
        return BeanTool.mapListToObjects(results, entityClass);
    }

    /**
     * Update multiple data objects by their IDs.
     * Return the updated object fetched from the database with the latest field values.
     *
     * @param objects the list of data objects to update
     * @param ignoreNull whether to ignore null values during the update
     * @return the updated objects fetched from the database with the latest field values.
     */
    @Override
    public List<T> updateListAndReturn(List<T> objects, boolean ignoreNull) {
        List<Map<String, Object>> rows = BeanTool.objectsToMapList(objects, ignoreNull);
        List<Map<String, Object>> results = modelService.updateListAndReturn(modelName, rows, ConvertType.TYPE_CAST);
        return BeanTool.mapListToObjects(results, entityClass);
    }

    /**
     * Delete one data object by id.
     *
     * @param id data id
     * @return true / Exception
     */
    @Override
    public boolean deleteOne(K id) {
        return modelService.deleteOne(modelName, id);
    }

    /**
     * Delete multiple data objects by ids.
     *
     * @param ids data ids
     * @return true / Exception
     */
    @Override
    public boolean deleteList(List<K> ids) {
        return modelService.deleteList(modelName, ids);
    }

    /**
     * Delete data objects by specified filters.
     *
     * @param filters filters
     * @return true / Exception
     */
    @Override
    public boolean deleteByFilters(Filters filters) {
        return modelService.deleteByFilters(modelName, filters);
    }

    /**
     * Get the ids based on the filters.
     *
     * @param filters filters
     * @return ids list
     */
    @Override
    public List<K> getIds(Filters filters) {
        return modelService.getIds(modelName, filters);
    }

    /**
     * Get the ids for ManyToOne/OneToOne relational field.
     *
     * @param filters filters
     * @param fieldName relational field name
     * @return distinct ids for relational field
     */
    @Override
    public <EK extends Serializable> List<EK> getRelatedIds(Filters filters, String fieldName) {
        return modelService.getRelatedIds(modelName, filters, fieldName);
    }

    /**
     * Query a single object based on filters. Only for code use.
     * Throw an exception when there are multiple objects.
     *
     * @param filters filters object
     * @return single object
     */
    @Override
    public T searchOne(Filters filters) {
        return this.searchOne(new FlexQuery(filters));
    }

    /**
     * Query a single object based on filters. Only for code use.
     * Throw an exception when there are multiple objects.
     *
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @return single object
     */
    @Override
    public T searchOne(FlexQuery flexQuery) {
        Map<String, Object> row = modelService.searchOne(modelName, flexQuery);
        return CollectionUtils.isEmpty(row) ? null : BeanTool.mapToObject(row, entityClass);
    }

    /**
     * Query objects without pagination, only for code use.
     * If the result exceeds the MAX_BATCH_SIZE, an error log is recorded, but no exception is thrown.
     *
     * @return data list
     */
    @Override
    public List<T> searchList() {
        return this.searchList(new FlexQuery());
    }

    /**
     * Query objects based on filters without pagination, only for code use.
     * If the result exceeds the MAX_BATCH_SIZE, an error log is recorded, but no exception is thrown.
     *
     * @param filters filters
     * @return object list
     */
    @Override
    public List<T> searchList(Filters filters) {
        return this.searchList(new FlexQuery(filters));
    }

    /**
     * Query objects based on FlexQuery without pagination, only for code use.
     * If the result exceeds the MAX_BATCH_SIZE, an error log is recorded, but no exception is thrown.
     *
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @return object list
     */
    @Override
    public List<T> searchList(FlexQuery flexQuery) {
        return modelService.searchList(modelName, flexQuery, entityClass);
    }

    /**
     * Searches for objects based on the provided FlexQuery and maps them to the specified DTO class.
     * If the result exceeds the MAX_BATCH_SIZE, an error log is recorded, but no exception is thrown.
     *
     * @param <R> the type of the DTO class
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @param dtoClass the class of the objects to be returned
     * @return object list of the specified DTO class
     */
    @Override
    public <R> List<R> searchList(FlexQuery flexQuery, Class<R> dtoClass) {
        return modelService.searchList(modelName, flexQuery, dtoClass);
    }

    /**
     * Query objects based on FlexQuery with pagination.
     * The page size cannot exceed the MAX_BATCH_SIZE.
     *
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @param page the Page object containing pagination information
     * @return a Page object containing the objects
     */
    @Override
    public Page<T> searchPage(FlexQuery flexQuery, Page<T> page) {
        return modelService.searchPage(modelName, flexQuery, page, entityClass);
    }

    /**
     * Query objects based on FlexQuery with pagination and map them to the specified DTO class.
     * The page size cannot exceed the MAX_BATCH_SIZE.
     *
     * @param <R> the type of the DTO class
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @param page the Page object containing pagination information
     * @param dtoClass the class of the objects to be returned
     * @return a Page object containing the DTO objects
     */
    @Override
    public <R> Page<R> searchPage(FlexQuery flexQuery, Page<R> page, Class<R> dtoClass) {
        return modelService.searchPage(modelName, flexQuery, page, dtoClass);
    }

    /**
     * Groups objects by their IDs based on the provided filters.
     * If the result exceeds the MAX_BATCH_SIZE, an error log is recorded, but no exception is thrown.
     *
     * @param filters the filters to apply when searching for objects
     * @return objects map (id -> object)
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
