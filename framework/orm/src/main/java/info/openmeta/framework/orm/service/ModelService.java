package info.openmeta.framework.orm.service;

import info.openmeta.framework.orm.domain.*;
import info.openmeta.framework.orm.enums.ConvertType;
import jakarta.validation.constraints.NotNull;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Common Model Service Interface.
 * The generic CRUD and query operations for a specified model.
 *
 * @param <K> id class
 */
public interface ModelService<K extends Serializable> {

    /**
     * Creates a new row in the specified model and returns its ID.
     *
     * @param modelName the name of the model
     * @param row the data to create
     * @return the ID of the newly created row
     */
    K createOne(String modelName, Map<String, Object> row);

    /**
     * Creates a new row in the specified model and returns the created data,
     * including any auto-generated fields, with the specified convert type.
     *
     * @param modelName the name of the model
     * @param row the data to create
     * @param convertType the conversion type applied to the result
     * @return a map containing the newly created row with updated field values
     */
    Map<String, Object> createOneAndFetch(String modelName, Map<String, Object> row, ConvertType convertType);

    /**
     * Creates multiple rows and returns a list of their IDs.
     *
     * @param modelName the name of the model
     * @param rows the list of data rows to create
     * @return a list of IDs corresponding to the newly created rows
     */
    List<K> createList(String modelName, List<Map<String, Object>> rows);

    /**
     * Creates multiple rows, returning the newly created data with any
     * auto-generated fields, and applying the specified convert type.
     *
     * @param modelName the name of the model
     * @param rows the list of data rows to create
     * @param convertType the conversion type applied to the results
     * @return a list of maps containing the newly created rows and their field values
     */
    List<Map<String, Object>> createListAndFetch(String modelName, List<Map<String, Object>> rows, ConvertType convertType);

    /**
     * Get a row by its ID. By default, all accessible fields are returned.
     * ManyToOne, OneToOne, Option, and MultiOption fields remain in their original form.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @return an {@link Optional} containing the row if found; otherwise empty
     */
    Optional<Map<String, Object>> getById(String modelName, K id);

    /**
     * Get a row by its ID with all fields by default, optionally expanding relational fields via subQueries.
     * ManyToOne, OneToOne, Option, and MultiOption fields remain in their original form.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @param subQueries a {@link SubQueries} object describing how to expand relational fields
     * @return an {@link Optional} containing the row if found; otherwise empty
     */
    Optional<Map<String, Object>> getById(String modelName, K id, SubQueries subQueries);

    /**
     * Get a row by its ID, optionally specifying which fields to return.
     * If no fields are specified, all accessible fields are returned by default.
     * ManyToOne, OneToOne, Option, and MultiOption fields remain in their original form.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @param fields the set of fields to retrieve
     * @return an {@link Optional} containing the row if found; otherwise empty
     */
    Optional<Map<String, Object>> getById(String modelName, K id, Collection<String> fields);

    /**
     * Get a row by its ID, specifying fields to return,
     * optionally expanding relational fields via subQueries, and applying a convert type.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @param fields the set of fields to retrieve; if null/empty, returns all accessible fields
     * @param subQueries a {@link SubQueries} object describing how to expand relational fields
     * @param convertType the conversion type applied to the result
     * @return an {@link Optional} containing the row if found; otherwise empty
     */
    Optional<Map<String, Object>> getById(String modelName, K id, Collection<String> fields,
                                SubQueries subQueries, ConvertType convertType);

    /**
     * Get multiple rows by their IDs, optionally specifying which fields to return.
     * If no fields are specified, all accessible fields are returned by default.
     * ManyToOne, OneToOne, Option, and MultiOption fields remain in their original form.
     *
     * @param modelName the name of the model
     * @param ids a list of row IDs
     * @param fields the set of fields to retrieve
     * @return a list of maps representing the retrieved rows
     */
    List<Map<String, Object>> getByIds(String modelName, List<K> ids, Collection<String> fields);

    /**
     * Get multiple rows by their IDs, optionally specifying which fields to return,
     * expanding relational fields via subQueries, and applying a convert type.
     *
     * @param modelName the name of the model
     * @param ids a list of row IDs
     * @param fields the set of fields to retrieve; if null/empty, returns all accessible fields
     * @param subQueries a {@link SubQueries} object describing how to expand relational fields
     * @param convertType the conversion type applied to the result
     * @return a list of maps representing the retrieved rows
     */
    List<Map<String, Object>> getByIds(String modelName, @NotNull List<K> ids, Collection<String> fields,
                                       SubQueries subQueries, ConvertType convertType);

    /**
     * Get all copyable fields from a row, identified by its ID,
     * without inserting any new data.
     *
     * @param modelName the name of the model
     * @param id the ID of the source row
     * @return a map containing values of all copyable fields
     */
    Map<String, Object> getCopyableFields(String modelName, K id);

    /**
     * Get display names for the specified IDs, returning a map of {id -> displayName}.
     * If no display fields are specified, uses the model's default 'displayName' configuration.
     *
     * @param modelName the name of the model
     * @param ids a list of row IDs
     * @param displayFields an optional list of fields to be used for display name
     * @return a map of IDs to their resolved display names
     */
    Map<K, String> getDisplayNames(String modelName, List<K> ids, List<String> displayFields);

    /**
     * Get distinct values for the specified field, filtered by the given conditions.
     *
     * @param <V> the type of the field's value
     * @param modelName the name of the model
     * @param field the field name for which to retrieve distinct values
     * @param filters optional filtering conditions
     * @return a list of distinct field values
     */
    <V extends Serializable> List<V> getDistinctFieldValue(String modelName, String field, Filters filters);

    /**
     * Get the value of a specified field from a row identified by its ID.
     * ManyToOne, OneToOne, Option, and MultiOption fields remain in their original form.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @param field the field name whose value to retrieve
     * @param <V> the type of the field's value
     * @return the field's value
     */
    <V extends Serializable> V getFieldValue(String modelName, K id, String field);

    /**
     * Get a list of IDs matching the given filters.
     *
     * @param modelName the name of the model
     * @param filters filtering conditions
     * @return a list of matching IDs
     */
    List<K> getIds(String modelName, Filters filters);

    /**
     * Get a distinct list of IDs for a ManyToOne/OneToOne relationship field
     * based on the given filters.
     *
     * @param <EK> the type of the related entity's ID
     * @param modelName the name of the model
     * @param filters filtering conditions
     * @param fieldName the name of the relationship field
     * @return a distinct list of related entity IDs
     */
    <EK extends Serializable> List<EK> getRelatedIds(String modelName, Filters filters, String fieldName);

    /**
     * Get the unmasked value of a specified field from a row identified by its ID.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @param fieldName the name of the masked field
     * @return the unmasked field value
     */
    String getUnmaskedField(String modelName, K id, String fieldName);

    /**
     * Get multiple unmasked field values from a row identified by its ID.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @param fields a list of masked fields to retrieve
     * @return a map of fieldName to unmasked value
     */
    Map<String, Object> getUnmaskedFields(String modelName, K id, List<String> fields);

    /**
     * Updates an existing row by its ID.
     *
     * @param modelName the name of the model
     * @param row the data containing updates
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    boolean updateOne(String modelName, Map<String, Object> row);

    /**
     * Updates an existing row by its ID, then fetches the updated row from the database.
     *
     * @param modelName the name of the model
     * @param row the data containing updates
     * @param convertType the conversion type applied to the result
     * @return a map representing the updated row with the latest field values
     */
    Map<String, Object> updateOneAndFetch(String modelName, Map<String, Object> row, ConvertType convertType);

    /**
     * Updates multiple rows by their IDs.
     * Each row in the list can specify different fields for update.
     *
     * @param modelName the name of the model
     * @param rows a list of data rows to update
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    boolean updateList(String modelName, List<Map<String, Object>> rows);

    /**
     * Updates multiple rows by their IDs and then fetches them from the database.
     * Each row in the list can specify different fields for update.
     *
     * @param modelName the name of the model
     * @param rows a list of data rows to update
     * @param convertType the conversion type applied to the results
     * @return a list of updated rows with the latest field values
     */
    List<Map<String, Object>> updateListAndFetch(String modelName, List<Map<String, Object>> rows, ConvertType convertType);

    /**
     * Updates a single row identified by its business key.
     *
     * @param modelName the name of the model
     * @param row the data containing updates, including business key fields
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    boolean updateByBusinessKey(String modelName, Map<String, Object> row);

    /**
     * Updates multiple rows identified by external IDs.
     * Each row in the list can specify different fields for update.
     *
     * @param modelName the name of the model
     * @param rows a list of data rows to update, including external IDs
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    boolean updateByExternalId(String modelName, List<Map<String, Object>> rows);

    /**
     * Performs a batch update of rows that match the provided filters,
     * updating the fields specified in the value map.
     * <p>If no filters are specified, all data visible to the current user might be updated.</p>
     *
     * @param modelName the name of the model
     * @param filters optional filter criteria
     * @param value a map of field-value pairs to update
     * @return the number of rows affected
     */
    Integer updateByFilter(String modelName, Filters filters, Map<String, Object> value);

    /**
     * Deletes a single row by its ID.
     * If the model is a timeline model, all slices related to this ID are also deleted.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    boolean deleteById(String modelName, K id);

    /**
     * Deletes a single slice of a timeline model, identified by sliceId (the model's primary key).
     *
     * @param modelName the name of the model
     * @param sliceId the slice's unique ID
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    boolean deleteBySliceId(String modelName, Long sliceId);

    /**
     * Deletes multiple rows by their IDs.
     *
     * @param modelName the name of the model
     * @param ids a list of unique IDs for the rows to delete
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    boolean deleteByIds(String modelName, List<K> ids);

    /**
     * Deletes a single row identified by its business key.
     *
     * @param modelName the name of the model
     * @param row the data containing the business key fields
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    boolean deleteByBusinessKey(String modelName, Map<String, Object> row);


    /**
     * Deletes a single row identified by its external ID.
     *
     * @param modelName the name of the model
     * @param externalId the external ID
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    boolean deleteByExternalId(String modelName, Serializable externalId);

    /**
     * Deletes multiple rows identified by their external IDs.
     *
     * @param modelName the name of the model
     * @param externalIds a list of external IDs
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    boolean deleteByExternalIds(String modelName, List<Serializable> externalIds);

    /**
     * Deletes rows matching the specified filters.
     *
     * @param modelName the name of the model
     * @param filters filtering conditions
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    boolean deleteByFilters(String modelName, Filters filters);

    /**
     * Copies a single row identified by its ID, returning the ID of the newly created copy.
     * <p>Read and create permissions are evaluated separately.</p>
     *
     * @param modelName the name of the model
     * @param id the source row's ID
     * @return the ID of the newly created copy
     */
    K copyById(String modelName, K id);

    /**
     * Copies a single row identified by its ID and returns the newly created data,
     * with the specified convert type applied.
     * <p>Read and create permissions are evaluated separately.</p>
     *
     * @param modelName the name of the model
     * @param id the source row's ID
     * @param convertType the conversion type applied to the result
     * @return a map representing the newly created row
     */
    Map<String, Object> copyByIdAndFetch(String modelName, K id, ConvertType convertType);

    /**
     * Copies multiple rows based on their IDs and returns the IDs of the new copies.
     * <p>Read and create permissions are evaluated separately.</p>
     *
     * @param modelName the name of the model
     * @param ids a list of source row IDs
     * @return a list of IDs corresponding to the newly created copies
     */
    List<K> copyByIds(String modelName, List<K> ids);

    /**
     * Copies multiple rows based on their IDs and returns the newly created data,
     * with the specified convert type applied.
     * <p>Read and create permissions are evaluated separately.</p>
     *
     * @param modelName the name of the model
     * @param ids a list of source row IDs
     * @param convertType the conversion type applied to the result
     * @return a list of maps representing the newly created rows
     */
    List<Map<String, Object>> copyByIdsAndFetch(String modelName, List<K> ids, ConvertType convertType);

    /**
     * Queries exactly one row matching the specified FlexQuery.
     * <p>If multiple rows match, an exception is thrown. This method is intended for internal code use.</p>
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} object defining fields, filters, sorting, etc.
     * @return a map representing the single matched row
     */
    Map<String, Object> searchOne(String modelName, FlexQuery flexQuery);

    /**
     * Performs a non-paginated query based on FlexQuery, intended for internal code use.
     * <p>If the result set exceeds {@code MAX_BATCH_SIZE}, an error is logged but no exception is thrown.</p>
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} object defining fields, filters, sorting, etc.
     * @return a list of maps representing the matching rows
     */
    List<Map<String, Object>> searchList(String modelName, FlexQuery flexQuery);

    /**
     * Performs a non-paginated query based on FlexQuery.
     * <p>If the result set exceeds {@code MAX_BATCH_SIZE}, an error is logged but no exception is thrown.</p>
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} object defining fields, filters, sorting, etc.
     * @return a list of maps representing the matching rows with displayName and ID
     */
    List<Map<String, Object>> searchName(String modelName, FlexQuery flexQuery);

    /**
     * Performs a non-paginated query based on FlexQuery and maps the results
     * to the specified return class.
     * <p>If the result set exceeds {@code MAX_BATCH_SIZE}, an error is logged
     * but no exception is thrown.</p>
     *
     * @param <R> the generic type of the result
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} object defining fields, filters, sorting, etc.
     * @param returnClass the class to map each row to
     * @return a list of objects mapped to the specified class
     */
    <R> List<R> searchList(String modelName, FlexQuery flexQuery, Class<R> returnClass);

    /**
     * Performs a paginated query based on FlexQuery, returning a {@link Page} of results.
     * <p>The page size must not exceed {@code MAX_BATCH_SIZE}.</p>
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} object defining fields, filters, sorting, etc.
     * @param page the {@link Page} containing pagination parameters
     * @return a {@link Page} of maps representing the queried rows
     */
    Page<Map<String, Object>> searchPage(String modelName, FlexQuery flexQuery, Page<Map<String, Object>> page);

    /**
     * Performs a paginated query based on FlexQuery, mapping each row to the specified return class.
     * <p>The page size must not exceed {@code MAX_BATCH_SIZE}.</p>
     *
     * @param <R> the generic type of the result
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} object defining fields, filters, sorting, etc.
     * @param page the {@link Page} containing pagination parameters
     * @param returnClass the class to map each row to
     * @return a {@link Page} of mapped objects
     */
    <R> Page<R> searchPage(String modelName, FlexQuery flexQuery, Page<R> page, Class<R> returnClass);

    /**
     * Queries data in a tree structure based on the specified FlexQuery.
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} object defining fields, filters, sorting, etc.
     * @return a list of maps representing the data in a hierarchical tree structure
     */
    List<Map<String, Object>> searchTree(String modelName, FlexQuery flexQuery);

    /**
     * Executes a PivotTable query based on the specified FlexQuery, returning up to
     * {@code DEFAULT_BATCH_SIZE} rows without pagination.
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} object defining fields, filters, sorting, grouping, splitting, summary, etc.
     * @return a {@link PivotTable} object encapsulating the pivot results
     */
    PivotTable searchPivot(String modelName, FlexQuery flexQuery);

    /**
     * Counts the number of rows matching the given filters.
     *
     * @param modelName the name of the model
     * @param filters filtering conditions
     * @return the total count of matching rows
     */
    Long count(String modelName, Filters filters);

    /**
     * Checks if a row with the specified ID physically exists in the underlying data store,
     * without performing any permission checks.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @return {@code true} if the row physically exists; otherwise false
     */
    boolean exist(String modelName, K id);

    /**
     * Filters the provided list of IDs, returning only those that physically exist
     * in the data store, without performing any permission checks.
     *
     * @param modelName the name of the model
     * @param ids the collection of IDs to check
     * @return a list of IDs that exist
     */
    List<K> filterExistIds(String modelName, Collection<K> ids);
}
