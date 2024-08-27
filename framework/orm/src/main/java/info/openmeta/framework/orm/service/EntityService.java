package info.openmeta.framework.orm.service;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.entity.BaseModel;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * Entity Service Interface.
 * The basic CRUD operations for the entity.
 *
 * @param <T> entity class
 * @param <K> id class
 */
public interface EntityService<T extends BaseModel, K extends Serializable> {

    /**
     * Create a single data object and return the id.
     *
     * @param object data object to be created
     * @return id
     */
    K createOne(T object);

    /**
     * Create a single data object, return the object with id and other latest field values
     *
     * @param object data object to be created
     * @return object with id and other latest field values.
     */
    T createOneAndReturn(T object);

    /**
     * Create multiple data objects and return the id list.
     *
     * @param objects data object list to be created
     * @return id list
     */
    List<K> createList(List<T> objects);

    /**
     * Create multiple data objects, return the object list with id and other latest field values.
     *
     * @param objects data object list to be created
     * @return object list, each object with id and other latest field values.
     */
    List<T> createListAndReturn(List<T> objects);

    /**
     * Read one data object by id.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id data id
     * @return data object
     */
    T readOne(K id);

    /**
     * Read one data object by id.
     * If the fields is not specified, all accessible fields as the default.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id data id
     * @param fields field list to read
     * @return data object
     */
    T readOne(K id, Collection<String> fields);

    /**
     * Read multiple data objects by ids.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param ids data ids list
     * @return data object list
     */
    List<T> readList(List<K> ids);

    /**
     * Read multiple data objects by ids.
     * If the fields is not specified, all accessible fields as the default.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param ids data ids list
     * @param fields field list to read
     * @return data object list
     */
    List<T> readList(List<K> ids, Collection<String> fields);

    /**
     * Update one data object by its ID. Null values are not ignored.
     *
     * @param object the data object to update
     * @return true / Exception
     */
    boolean updateOne(T object);

    /**
     * Update one data object by its ID.
     *
     * @param object the data object to update
     * @param ignoreNull whether to ignore null values during the update
     * @return true / Exception
     */
    boolean updateOne(T object, boolean ignoreNull);

    /**
     * Update one data object by its ID. Null values are not ignored.
     * Return the updated object fetched from the database with the latest field values.
     *
     * @param object the data object to update
     * @return the updated object fetched from the database with the latest field values.
     */
    T updateOneAndReturn(T object);

    /**
     * Update one data object by its ID.
     * Return the updated object fetched from the database with the latest field values.
     *
     * @param object the data object to update
     * @param ignoreNull whether to ignore null values during the update
     * @return the updated object fetched from the database, with the latest field values.
     */
    T updateOneAndReturn(T object, boolean ignoreNull);

    /**
     * Update multiple data objects by their IDs. Null values are not ignored.
     *
     * @param objects the list of data objects to update
     * @return true / Exception
     */
    boolean updateList(List<T> objects);

    /**
     * Update multiple data objects by their IDs.
     *
     * @param objects the list of data objects to update
     * @param ignoreNull whether to ignore null values during the update
     * @return true / Exception
     */
    boolean updateList(List<T> objects, boolean ignoreNull);

    /**
     * Update multiple data objects by their IDs. Null values are not ignored.
     * Return the updated object fetched from the database with the latest field values.
     *
     * @param objects the list of data objects to update
     * @return the updated objects fetched from the database with the latest field values.
     */
    List<T> updateListAndReturn(List<T> objects);

    /**
     * Update multiple data objects by their IDs.
     * Return the updated object fetched from the database with the latest field values.
     *
     * @param objects the list of data objects to update
     * @param ignoreNull whether to ignore null values during the update
     * @return the updated objects fetched from the database with the latest field values.
     */
    List<T> updateListAndReturn(List<T> objects, boolean ignoreNull);

    /**
     * Delete one data object by ID.
     *
     * @param id data id
     * @return true / Exception
     */
    boolean deleteOne(K id);

    /**
     * Delete multiple data objects by IDs.
     *
     * @param ids data ids
     * @return true / Exception
     */
    boolean deleteList(List<K> ids);

    /**
     * Delete data objects by specified filters.
     *
     * @param filters filter conditions
     * @return true / Exception
     */
    boolean deleteByFilters(Filters filters);

    /**
     * Get the ids based on the filters.
     *
     * @param filters filters
     * @return ids list
     */
    List<K> getIds(Filters filters);

    /**
     * Get the ids for ManyToOne/OneToOne relational field.
     *
     * @param filters filters
     * @param fieldName relational field name
     * @return distinct ids for relational field
     */
    List<K> getRelatedIds(Filters filters, String fieldName);

    /**
     * Query a single object based on filters. Only for code use.
     * Throw an exception when there are multiple objects.
     *
     * @param filters filters object
     * @return single object
     */
    T searchOne(Filters filters);

    /**
     * Query a single object based on filters. Only for code use.
     * Throw an exception when there are multiple objects.
     *
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @return single object
     */
    T searchOne(FlexQuery flexQuery);

    /**
     * Query objects without pagination, only for code use.
     * If the result exceeds the MAX_BATCH_SIZE, an error log is recorded, but no exception is thrown.
     *
     * @return object list
     */
    List<T> searchList();

    /**
     * Query objects based on Filters without pagination, only for code use.
     * If the result exceeds the MAX_BATCH_SIZE, an error log is recorded, but no exception is thrown.
     *
     * @param filters filters
     * @return object list
     */
    List<T> searchList(Filters filters);

    /**
     * Query objects based on FlexQuery without pagination, only for code use.
     * If the result exceeds the MAX_BATCH_SIZE, an error log is recorded, but no exception is thrown.
     *
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @return object list
     */
    List<T> searchList(FlexQuery flexQuery);

    /**
     * Searches for objects based on the provided FlexQuery and maps them to the specified DTO class.
     * If the result exceeds the MAX_BATCH_SIZE, an error log is recorded, but no exception is thrown.
     *
     * @param <R> the type of the DTO class
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @param dtoClass the class of the objects to be returned
     * @return object list of the specified DTO class
     */
    <R> List<R> searchList(FlexQuery flexQuery, Class<R> dtoClass);

    /**
     * Query objects based on FlexQuery with pagination.
     * The page size cannot exceed the MAX_BATCH_SIZE.
     *
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @param page the Page object containing pagination information
     * @return a Page object containing the objects
     */
    Page<T> searchPage(FlexQuery flexQuery, Page<T> page);

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
    <R> Page<R> searchPage(FlexQuery flexQuery, Page<R> page, Class<R> dtoClass);

    /**
     * Groups objects by their ID based on the provided filters.
     * If the result exceeds the MAX_BATCH_SIZE, an error log is recorded, but no exception is thrown.
     *
     * @param filters the filters to apply when searching for objects
     * @return objects map (id -> object)
     */
    Map<Long, T> groupById(Filters filters);

}
