package info.openmeta.framework.orm.jdbc;

import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.enums.ConvertType;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * JdbcService
 * @param <K> Primary key type
 */
public interface JdbcService<K extends Serializable> {

    /**
     * Batch creates data and returns the result.
     *
     * @param rows List of data to be created
     * @return List<Map> results
     */
    List<Map<String, Object>> insertList(String modelName, List<Map<String, Object>> rows);

    /**
     * Get multiple rows by ids.
     * If the fields are not specified, all accessible fields as the default.
     *
     * @param modelName Model name
     * @param ids    List of data ids
     * @param fields Field list
     * @param convertType Data convert type
     * @return List<Map> of multiple data
     */
    List<Map<String, Object>> selectByIds(String modelName, List<K> ids, List<String> fields, ConvertType convertType);

    /**
     * Get multiple rows by flexQuery object.
     *
     * @param modelName Model name
     * @param flexQuery flexQuery
     * @return List<Map> of multiple data
     */
    List<Map<String, Object>> selectByFilter(String modelName, FlexQuery flexQuery);

    /**
     * Get the ids by flexQuery object.
     *
     * @param modelName Model name
     * @param fieldName Default to get the ids of the current model,
     *                  or specify a ManyToOne or OneToOne fieldName to get the ids of this field.
     * @param flexQuery flexQuery
     * @return List<K>
     */
    <EK extends Serializable> List<EK> getIds(String modelName, String fieldName, FlexQuery flexQuery);

    /**
     * Determine if the id physically exists, without permission check.
     *
     * @param modelName Model name
     * @param id Data id
     * @return true or false
     */
    boolean exist(String modelName, Serializable id);

    /**
     * Query data based on FlexQuery with pagination.
     *
     * @param modelName Model name
     * @param flexQuery flexQuery
     * @return Page<Map> of multiple data
     */
    Page<Map<String, Object>> selectByPage(String modelName, FlexQuery flexQuery, Page<Map<String, Object>> page);

    /**
     * Update one row by id, not including data conversion, fill in audit fields, and send changelog.
     * Only for limited use.
     *
     * @param modelName Model name
     * @param rowMap Update data
     * @return Number of affected rows
     */
    Integer updateOne(String modelName, Map<String, Object> rowMap);

    /**
     * Batch update data by id, support different keys in different Map.
     *
     * @param modelName Model name
     * @param rows List of data to be updated
     * @return Number of affected rows
     */
    Integer updateList(String modelName, List<Map<String, Object>> rows, Set<String> toUpdateFields);

    /**
     * Delete a slice of timeline model by `sliceId`.
     *
     * @param modelName Model name
     * @param sliceId Slice id of timeline model
     * @return true / Exception
     */
    boolean deleteBySliceId(String modelName, Serializable sliceId);

    /**
     * Delete multiple rows by ids.
     *
     * @param modelName Model name
     * @param ids Data ids
     * @param rows Deleted data rows, used to send changelog
     * @return true / Exception
     */
    boolean deleteByIds(String modelName, List<K> ids, List<Map<String, Object>> rows);

    /**
     * Count by flexQuery object.
     *
     * @param modelName Model name
     * @param flexQuery flexQuery
     * @return count result
     */
    Long count(String modelName, FlexQuery flexQuery);

    /**
     * Query data by class object, not using the metadata of modelManager.
     *
     * @param entityClass Entity class
     * @param orderBy Order by
     * @return Object list
     */
    <T> List<T> selectMetaEntityList(Class<T> entityClass, String orderBy);

    /**
     * Query data by model and class object, not using the metadata of modelManager.
     *
     * @param model Model name
     * @param entityClass Entity class
     * @param orderBy Order by
     * @return Object list
     */
    <T> List<T> selectMetaEntityList(String model, Class<T> entityClass, String orderBy);

}
