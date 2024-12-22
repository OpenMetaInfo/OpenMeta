package info.openmeta.framework.orm.service;

import info.openmeta.framework.orm.domain.*;
import info.openmeta.framework.orm.enums.ConvertType;
import jakarta.validation.constraints.NotNull;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * Common Model Service Interface.
 * The basic CRUD operations for the model.
 *
 * @param <K> id class
 */
public interface ModelService<K extends Serializable> {

    /**
     * Create a single row and return the id.
     *
     * @param modelName model name
     * @param row data row to be created
     * @return id
     */
    K createOne(String modelName, Map<String, Object> row);

    /**
     * Create a single row, fetch the row data after creation.
     *
     * @param modelName model name
     * @param row data row to be created
     * @param convertType data convert type of the return value.
     * @return row data with latest field values
     */
    Map<String, Object> createOneAndFetch(String modelName, Map<String, Object> row, ConvertType convertType);

    /**
     * Create multiple rows and return the id list.
     *
     * @param modelName model name
     * @param rows data rows to be created
     * @return id list
     */
    List<K> createList(String modelName, List<Map<String, Object>> rows);

    /**
     * Create multiple rows, fetch the rows after creation.
     *
     * @param modelName model name
     * @param rows data rows to be created
     * @param convertType data convert type of the return value.
     * @return row data list with id and other latest field values
     */
    List<Map<String, Object>> createListAndFetch(String modelName, List<Map<String, Object>> rows, ConvertType convertType);

    /**
     * Get one row by id, default to read all fields.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id data id
     * @return data row
     */
    Map<String, Object> getById(String modelName, K id);

    /**
     * Get one row by id, default to read all fields.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id data id
     * @param subQueries subQueries for relational fields
     * @return data row
     */
    Map<String, Object> getById(String modelName, K id, SubQueries subQueries);

    /**
     * Get one row by id.
     * If the fields is not specified, all accessible fields as the default.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id data id
     * @param fields field list to get value
     * @return data row
     */
    Map<String, Object> getById(String modelName, K id, Collection<String> fields);

    /**
     * Get one row by id.
     * If the fields is not specified, all accessible fields as the default.
     *
     * @param id data id
     * @param fields field list to get value
     * @param subQueries subQueries for relational fields
     * @param convertType data convert type of the return value.
     * @return data row
     */
    Map<String, Object> getById(String modelName, K id, Collection<String> fields,
                                SubQueries subQueries, ConvertType convertType);

    /**
     * Get multiple rows by ids.
     * If the fields is not specified, all accessible fields as the default.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param ids List of data ids
     * @param fields Field list to get value
     * @return List<Map> of multiple data
     */
    List<Map<String, Object>> getByIds(String modelName, List<K> ids, Collection<String> fields);

    /**
     * Get multiple rows by ids.
     * If the fields is not specified, all accessible fields as the default.
     *
     * @param ids List of data ids
     * @param fields Field list to get value
     * @param subQueries subQueries for relational fields
     * @param convertType data convert type of the return value.
     * @return List<Map> of multiple data
     */
    List<Map<String, Object>> getByIds(String modelName, @NotNull List<K> ids, Collection<String> fields,
                                       SubQueries subQueries, ConvertType convertType);

    /**
     * Get the copyable fields value by ID, no data inserted.
     *
     * @param modelName model name
     * @param id source data id
     * @return map of copyable field values
     */
    Map<String, Object> getCopyableFields(String modelName, K id);

    /**
     * Get the `displayNames` of the specified ids, returning map of {id: displayName}.
     * If the `displayFields` is not specified, use the `displayName` configuration of the model.
     *
     * @param modelName model name
     * @param ids id list
     * @param displayFields specified display field list.
     * @return displayNames Map
     */
    Map<K, String> getDisplayNames(String modelName, List<K> ids, List<String> displayFields);

    /**
     * Get the distinct field value list based on the filters.
     *
     * @param modelName model name
     * @param field field name to read and distinct
     * @param filters filters
     * @return distinct field value list
     */
    List<Object> getDistinctFieldValue(String modelName, String field, Filters filters);

    /**
     * Get the specified field value based on id.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id data id
     * @param field field name to get value
     * @return field value
     */
    <V extends Serializable> V getFieldValue(String modelName, K id, String field);

    /**
     * Get the ids based on the filters.
     *
     * @param modelName model name
     * @param filters filter conditions
     * @return ids list
     */
    List<K> getIds(String modelName, Filters filters);

    /**
     * Get the ids for ManyToOne/OneToOne relational field.
     *
     * @param modelName model name
     * @param filters filters
     * @param fieldName relational field name
     * @return distinct ids for relational field
     */
    <EK extends Serializable> List<EK> getRelatedIds(String modelName, Filters filters, String fieldName);

    /**
     * Get the unmasked field value based on the id and field name.
     *
     * @param modelName model name
     * @param id data id
     * @param fieldName masking field name
     * @return unmasked field value
     */
    String getUnmaskedField(String modelName, K id, String fieldName);

    /**
     * Get multiple unmasked field values based on the id and field names.
     *
     * @param modelName model name
     * @param id data id
     * @param fields masking field names
     * @return unmasked field values map, fieldName -> fieldValue
     */
    Map<String, Object> getUnmaskedFields(String modelName, K id, List<String> fields);

    /**
     * Update one row by id.
     *
     * @param row data row to be updated
     * @return true / Exception
     */
    boolean updateOne(String modelName, Map<String, Object> row);

    /**
     * Update one row by id, and fetch the updated row from database.
     *
     * @param modelName model name
     * @param row data row to be updated
     * @param convertType data convert type of the return value.
     * @return map of updated row data with the latest field values
     */
    Map<String, Object> updateOneAndFetch(String modelName, Map<String, Object> row, ConvertType convertType);

    /**
     * Update multiple rows by id. Each row in the list can have different fields.
     *
     * @param rows data rows to be updated
     * @return true / Exception
     */
    boolean updateList(String modelName, List<Map<String, Object>> rows);

    /**
     * Update multiple rows by ids. Each row in the list can have different fields.
     * And fetch the updated rows fetched from the database, with the latest field values.
     *
     * @param modelName model name
     * @param rows data rows to be updated
     * @param convertType data convert type of the return value.
     * @return updated rows with the latest field values
     */
    List<Map<String, Object>> updateListAndFetch(String modelName, List<Map<String, Object>> rows, ConvertType convertType);

    /**
     * Update multiple rows by externalIds. Each row in the list can have different fields.
     *
     * @param rows data rows to be updated
     * @return true / Exception
     */
    boolean updateByExternalId(String modelName, List<Map<String, Object>> rows);

    /**
     * Batch edit data based on the filters, according to the specified field values map.
     *
     * @param filters filters, if not specified, all visible data of the current user will be updated.
     * @param value field values to be updated
     * @return number of affected rows
     */
    Integer updateByFilter(String modelName, Filters filters, Map<String, Object> value);

    /**
     * Delete one row by id.
     * All the slices related to this `id` will be deleted if the model is a timeline model.
     *
     * @param id data id
     * @return true / Exception
     */
    boolean deleteById(String modelName, K id);

    /**
     * Delete a slice of timeline model by `sliceId`, the primary key of timeline model.
     *
     * @param modelName model name
     * @param sliceId data id
     * @return true / Exception
     */
    boolean deleteBySliceId(String modelName, Long sliceId);

    /**
     * Delete multiple rows by ids.
     *
     * @param ids data ids
     * @return true / Exception
     */
    boolean deleteByIds(String modelName, List<K> ids);

    /**
     * Delete multiple rows by externalIds.
     *
     * @param externalIds externalId List
     * @return true / Exception
     */
    boolean deleteByExternalIds(String modelName, List<Serializable> externalIds);

    /**
     * Delete rows by specified filters.
     *
     * @param modelName model name
     * @param filters filters
     * @return true / Exception
     */
    boolean deleteByFilters(String modelName, Filters filters);

    /**
     * Copy a single row based on id, and return the new id of the new row.
     * Currently, read and create permissions are checked separately
     *
     * @param modelName model name
     * @param id source data id
     * @return id of the new data
     */
    K copyById(String modelName, K id);

    /**
     * Copy a single row based on id, and fetch the new row.
     * Currently, read and create permissions are checked separately
     *
     * @param modelName model name
     * @param id source data id
     * @param convertType data convert type of the return value.
     * @return new row data
     */
    Map<String, Object> copyByIdAndFetch(String modelName, K id, ConvertType convertType);

    /**
     * Copy multiple rows based on ids, and return the ids of the new rows.
     * Currently, read and create permissions are checked separately
     *
     * @param modelName model name
     * @param ids source data ids
     * @return ids of the new data
     */
    List<K> copyByIds(String modelName, List<K> ids);

    /**
     * Copy multiple rows based on ids, and fetch the new rows.
     * Currently, read and create permissions are checked separately
     *
     * @param modelName model name
     * @param ids source data ids
     * @param convertType data convert type of the return value.
     * @return new row data list
     */
    List<Map<String, Object>> copyByIdsAndFetch(String modelName, List<K> ids, ConvertType convertType);

    /**
     * Query single row data based on filters. Only for code use.
     * Throw an exception when there are multiple objects.
     *
     * @param modelName model name
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @return single row data
     */
    Map<String, Object> searchOne(String modelName, FlexQuery flexQuery);

    /**
     * Query data list based on FlexQuery without pagination, only for code use.
     * If the result exceeds the MAX_BATCH_SIZE, an error log is recorded, but no exception is thrown.
     *
     * @param modelName model name
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @return data list
     */
    List<Map<String, Object>> searchList(String modelName, FlexQuery flexQuery);

    /**
     * Searches for objects based on the provided FlexQuery and maps them to the specified return class.
     * If the result exceeds the MAX_BATCH_SIZE, an error log is recorded, but no exception is thrown.
     *
     * @param <R> the type of the return class
     * @param modelName model name
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @param returnClass the class of the objects to be returned
     * @return object list of the specified return class
     */
    <R> List<R> searchList(String modelName, FlexQuery flexQuery, Class<R> returnClass);

    /**
     * Query data list based on FlexQuery with pagination.
     * The page size cannot exceed the MAX_BATCH_SIZE.
     *
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @param page the Page object containing pagination information
     * @return a Page object containing the Map data list
     */
    Page<Map<String, Object>> searchPage(String modelName, FlexQuery flexQuery, Page<Map<String, Object>> page);

    /**
     * Query objects based on FlexQuery with pagination.
     * The page size cannot exceed the MAX_BATCH_SIZE.
     *
     * @param <R> the type of the return class
     * @param modelName model name
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @param page the Page object containing pagination information
     * @param returnClass the class of the objects to be returned
     * @return a Page object containing the queried objects
     */
    <R> Page<R> searchPage(String modelName, FlexQuery flexQuery, Page<R> page, Class<R> returnClass);

    /**
     * Return data in a tree structure.
     *
     * @param flexQuery search conditions, can set fields, filters, orders, etc.
     * @return Tree structure data list
     */
    List<Map<String, Object>> searchTree(String modelName, FlexQuery flexQuery);

    /**
     * Query the PivotTable, without pagination, the data is limited to no more than DEFAULT_BATCH_SIZE records.
     *
     * @param flexQuery search conditions, can set fields, filters, orders, groupBy, splitBy, summary, etc.
     * @return PivotTable object
     */
    PivotTable searchPivot(String modelName, FlexQuery flexQuery);

    /**
     * Count the number of records based on the filter conditions.
     *
     * @param modelName model name
     * @param filters filter conditions
     * @return count result
     */
    Long count(String modelName, Filters filters);

    /**
     * Determine if the id physically exists, without permission check.
     *
     * @param modelName Model name
     * @param id Data id
     * @return true or false
     */
    boolean exist(String modelName, K id);

    /**
     * Filter the set that exist in the database from ids, without permission check.
     *
     * @param modelName Model name
     * @param ids Data ids
     * @return ids that exist in the database
     */
    List<K> filterExistIds(String modelName, Collection<K> ids);
}
