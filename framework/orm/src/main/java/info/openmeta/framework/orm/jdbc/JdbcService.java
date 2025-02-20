package info.openmeta.framework.orm.jdbc;

import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.enums.ConvertType;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * A JDBC-based service interface providing operations for batch inserts, queries,
 * updates, and deletes on a specified model.
 *
 * @param <K> the type of the primary key
 */
public interface JdbcService<K extends Serializable> {

    /**
     * Performs a batch insertion of multiple rows into the specified model,
     * returning the inserted data (including any auto-generated fields).
     *
     * @param modelName the name of the model
     * @param rows a list of rows to be inserted
     * @return a list of maps representing the inserted rows
     */
    List<Map<String, Object>> insertList(String modelName, List<Map<String, Object>> rows);

    /**
     * Queries multiple rows by their IDs. If no fields are specified,
     * all accessible fields are returned by default.
     *
     * @param modelName the name of the model
     * @param ids a list of row IDs
     * @param fields the list of fields to retrieve; if null or empty, retrieves all accessible fields
     * @param convertType the type of data conversion applied to the result
     * @return a list of maps representing the retrieved rows
     */
    List<Map<String, Object>> selectByIds(String modelName, List<K> ids, List<String> fields, ConvertType convertType);

    /**
     * Queries multiple rows based on a given {@link FlexQuery}.
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} defining filters, fields, sorting, etc.
     * @return a list of maps representing the filtered rows
     */
    List<Map<String, Object>> selectByFilter(String modelName, FlexQuery flexQuery);

    /**
     * Get a list of IDs based on a given {@link FlexQuery}.
     * <p>If {@code fieldName} references the model's own primary key, retrieves IDs from the current model.
     * If it references a ManyToOne or OneToOne field, retrieves the related entity IDs.</p>
     *
     * @param <EK> the type of the retrieved IDs
     * @param modelName the name of the model
     * @param fieldName the field name whose IDs are retrieved
     * @param flexQuery a {@link FlexQuery} defining filters, sorting, etc.
     * @return a list of IDs of type {@code EK}
     */
    <EK extends Serializable> List<EK> getIds(String modelName, String fieldName, FlexQuery flexQuery);

    /**
     * Checks whether a row with the specified ID exists in the data store,
     * without performing any permission checks.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @return {@code true} if the row physically exists; otherwise {@code false}
     */
    boolean exist(String modelName, Serializable id);

    /**
     * Performs a paginated query based on a given {@link FlexQuery},
     * returning a {@link Page} of rows.
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} defining filters, fields, sorting, etc.
     * @param page the {@link Page} containing pagination parameters
     * @return a {@link Page} of maps representing the queried rows
     */
    Page<Map<String, Object>> selectByPage(String modelName, FlexQuery flexQuery, Page<Map<String, Object>> page);

    /**
     * Updates a single row identified by its ID, without performing any data conversion
     * or automatic field population. Also triggers the audit fields and change log as needed.
     * <p>Use with caution or in restricted scenarios.</p>
     *
     * @param modelName the name of the model
     * @param rowMap the data to update, including the row's primary key
     * @return the number of rows affected
     */
    Integer updateOne(String modelName, Map<String, Object> rowMap);

    /**
     * Performs a batch update of multiple rows by their IDs. Each row can
     * contain a different set of fields. Only the specified fields in
     * {@code toUpdateFields} will be updated if provided.
     *
     * @param modelName the name of the model
     * @param rows the list of rows to be updated
     * @param toUpdateFields a set of field names to update; if null/empty, updates all provided fields
     * @return the number of rows affected
     */
    Integer updateList(String modelName, List<Map<String, Object>> rows, Set<String> toUpdateFields);

    /**
     * Deletes a single slice from a timeline model, identified by its slice ID.
     *
     * @param modelName the name of the model
     * @param sliceId the unique slice ID in a timeline model
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    boolean deleteBySliceId(String modelName, Serializable sliceId);

    /**
     * Deletes multiple rows by their IDs. The {@code rows} parameter can be used
     * to send a change log or audit trail, if applicable.
     *
     * @param modelName the name of the model
     * @param ids a list of row IDs to delete
     * @param rows the list of data corresponding to the rows to be deleted
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    boolean deleteByIds(String modelName, List<K> ids, List<Map<String, Object>> rows);

    /**
     * Counts the number of rows matching the given {@link FlexQuery}.
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} defining filters, fields, etc.
     * @return the total count of matching rows
     */
    Long count(String modelName, FlexQuery flexQuery);

    /**
     * Retrieves data as a list of entities of the specified class,
     * not relying on any metadata from the model manager.
     *
     * @param <T> the type of the entity
     * @param entityClass the target entity class
     * @param orderBy an optional "ORDER BY" clause
     * @return a list of entities
     */
    <T> List<T> selectMetaEntityList(Class<T> entityClass, String orderBy);

    /**
     * Retrieves data as a list of entities of the specified class from the given model,
     * not relying on any metadata from the model manager.
     *
     * @param <T> the type of the entity
     * @param modelName the name of the model
     * @param entityClass the target entity class
     * @param orderBy an optional "ORDER BY" clause
     * @return a list of entities
     */
    <T> List<T> selectMetaEntityList(String modelName, Class<T> entityClass, String orderBy);

}
