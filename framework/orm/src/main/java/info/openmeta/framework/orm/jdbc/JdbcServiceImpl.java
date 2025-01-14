package info.openmeta.framework.orm.jdbc;

import com.google.common.collect.Sets;
import info.openmeta.framework.base.exception.VersionException;
import info.openmeta.framework.base.utils.Cast;
import info.openmeta.framework.orm.annotation.SkipPermissionCheck;
import info.openmeta.framework.orm.changelog.ChangeLogPublisher;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.enums.IdStrategy;
import info.openmeta.framework.orm.jdbc.database.SqlBuilderFactory;
import info.openmeta.framework.orm.jdbc.database.SqlParams;
import info.openmeta.framework.orm.jdbc.database.StaticSqlBuilder;
import info.openmeta.framework.orm.jdbc.pipeline.DataCreatePipeline;
import info.openmeta.framework.orm.jdbc.pipeline.DataReadPipeline;
import info.openmeta.framework.orm.jdbc.pipeline.DataUpdatePipeline;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.utils.BeanTool;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * JdbcServiceImpl
 * Don't check permissions at this layer, the upper layer has already checked permissions.
 *
 * @param <K> Primary key type
 */
@Slf4j
@Service
public class JdbcServiceImpl<K extends Serializable> implements JdbcService<K> {

    @Autowired
    private JdbcProxy jdbcProxy;

    @Autowired
    private ChangeLogPublisher changeLogPublisher;

    /**
     * Extracts all values of the List<Map> structure in the order of Fields and returns an array list.
     *
     * @param fields Field name list
     * @param rows Data list
     * @return List of field value arrays
     */
    private List<Object[]> getBatchValues(List<String> fields, List<Map<String, Object>> rows) {
        // The list of field value arrays
        List<Object[]> batchValues = new ArrayList<>(rows.size());
        for (Map<String, Object> value : rows) {
            Object[] values = new Object[fields.size()];
            for (int i = 0; i < fields.size(); i++) {
                values[i] = value.get(fields.get(i));
            }
            batchValues.add(values);
        }
        return batchValues;
    }

    /**
     * Performs a batch insertion of multiple rows into the specified model,
     * returning the inserted data (including any auto-generated fields).
     *
     * @param modelName the name of the model
     * @param rows a list of rows to be inserted
     * @return a list of maps representing the inserted rows
     */
    @SkipPermissionCheck
    public List<Map<String, Object>> insertList(String modelName, List<Map<String, Object>> rows) {
        LocalDateTime insertTime = LocalDateTime.now();
        DataCreatePipeline pipeline = new DataCreatePipeline(modelName);
        // Format the data, fill in the audit fields,
        // and fill in the tenantId field according to whether it is a multi-tenant model.
        rows = pipeline.processCreateData(rows, insertTime);
        List<String> fields = new ArrayList<>(pipeline.getStoredFields());
        // Get SQL
        SqlParams sqlParams = StaticSqlBuilder.getInsertSql(modelName, fields);
        IdStrategy idStrategy = ModelManager.getIdStrategy(modelName);
        if (IdStrategy.DB_AUTO_ID.equals(idStrategy)) {
            // When using database auto-increment id, the JDBCTemplate needs to be created one by one
            rows.forEach(row -> {
                List<Object> valueList = fields.stream().map(row::get).collect(Collectors.toList());
                sqlParams.setArgs(valueList);
                Serializable id = jdbcProxy.insert(modelName, sqlParams);
                row.put(ModelConstant.ID, id);
            });
        } else {
            // When the idStrategy is not DB_AUTO_ID, the id in the data has been assigned a value by default,
            // and the insert can be batch executed without the database returning ids
            List<Object[]> batchValues = getBatchValues(fields, rows);
            jdbcProxy.batchUpdate(modelName, sqlParams, batchValues);
        }
        // Collect changeLogs after filling in the id
        changeLogPublisher.publishCreationLog(modelName, rows, insertTime);
        // Because its need to get the ids first, and finally process the OneToMany, ManyToMany parameters,
        // to create associated table or intermediate table rows.
        pipeline.processXToManyData(rows);
        return rows;
    }

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
    @SkipPermissionCheck
    public List<Map<String, Object>> selectByIds(String modelName, List<K> ids, List<String> fields, ConvertType convertType) {
        String primaryKey = ModelManager.getModelPrimaryKey(modelName);
        if (CollectionUtils.isEmpty(fields)) {
            fields = ModelManager.getModelStoredFields(modelName);
        } else if (!fields.contains(primaryKey)) {
            fields.add(primaryKey);
        }
        SqlParams sqlParams = StaticSqlBuilder.getSelectSql(modelName, fields, ids);
        List<Map<String, Object>> rows = jdbcProxy.queryForList(modelName, sqlParams);
        if (CollectionUtils.isEmpty(rows)) {
            return Collections.emptyList();
        }
        // Whether to format the data, including data decryption, convert data, and fill in relation fields
        if (!ConvertType.ORIGINAL.equals(convertType)) {
            FlexQuery flexQuery = new FlexQuery(fields);
            flexQuery.setConvertType(convertType);
            DataReadPipeline dataPipeline = new DataReadPipeline(modelName, flexQuery);
            dataPipeline.processReadData(rows);
        }
        return rows;
    }

    /**
     * Queries multiple rows based on a given {@link FlexQuery}.
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} defining filters, fields, sorting, etc.
     * @return a list of maps representing the filtered rows
     */
    @SkipPermissionCheck
    public List<Map<String, Object>> selectByFilter(String modelName, FlexQuery flexQuery) {
        SqlParams sqlParams;
        if (flexQuery.getTopN() != null && flexQuery.getTopN() > 0) {
            sqlParams = SqlBuilderFactory.buildTopNSql(modelName, flexQuery);
        } else {
            sqlParams = SqlBuilderFactory.buildSelectSql(modelName, flexQuery);
        }
        List<Map<String, Object>> rows = jdbcProxy.queryForList(modelName, sqlParams);
        if (CollectionUtils.isEmpty(rows)) {
            return Collections.emptyList();
        }
        if (!ConvertType.ORIGINAL.equals(flexQuery.getConvertType())) {
            DataReadPipeline dataPipeline = new DataReadPipeline(modelName, flexQuery);
            dataPipeline.processReadData(rows);
        }
        return rows;
    }

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
    @SkipPermissionCheck
    public <EK extends Serializable> List<EK> getIds(String modelName, String fieldName, FlexQuery flexQuery) {
        flexQuery.setFields(Sets.newHashSet(fieldName));
        SqlParams sqlParams = SqlBuilderFactory.buildSelectSql(modelName, flexQuery);
        List<Map<String, Object>> rows = jdbcProxy.queryForList(modelName, sqlParams);
        return Cast.of(rows.stream()
                .map(row -> row.get(fieldName))
                .collect(Collectors.toList()));
    }

    /**
     * Checks whether a row with the specified ID exists in the data store,
     * without performing any permission checks.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @return {@code true} if the row physically exists; otherwise {@code false}
     */
    public boolean exist(String modelName, Serializable id) {
        Long count = this.count(modelName, new FlexQuery(new Filters().eq(ModelConstant.ID, id)));
        return count != null && count > 0;
    }

    /**
     * Performs a paginated query based on a given {@link FlexQuery},
     * returning a {@link Page} of rows.
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} defining filters, fields, sorting, etc.
     * @param page the {@link Page} containing pagination parameters
     * @return a {@link Page} of maps representing the queried rows
     */
    @SkipPermissionCheck
    public Page<Map<String, Object>> selectByPage(String modelName, FlexQuery flexQuery, Page<Map<String, Object>> page) {
        if (page.isCount()) {
            Long total = this.count(modelName, flexQuery);
            if (total == 0) {
                return page;
            }
            page.setTotal(total);
        }
        SqlParams sqlParams = SqlBuilderFactory.buildPageSql(modelName, flexQuery, page);
        List<Map<String, Object>> rows = jdbcProxy.queryForList(modelName, sqlParams);
        if (CollectionUtils.isEmpty(rows)) {
            return page;
        }
        // Whether to format the data, including data decryption, convert data, and fill in relation fields
        if (!ConvertType.ORIGINAL.equals(flexQuery.getConvertType())) {
            DataReadPipeline dataPipeline = new DataReadPipeline(modelName, flexQuery);
            dataPipeline.processReadData(rows);
        }
        return page.setRows(rows);
    }

    /**
     * Updates a single row identified by its ID, without performing any data conversion
     * or automatic field population. Also triggers the audit fields and change log as needed.
     * <p>Use with caution or in restricted scenarios.</p>
     *
     * @param modelName the name of the model
     * @param rowMap the data to update, including the row's primary key
     * @return the number of rows affected
     */
    public Integer updateOne(String modelName, Map<String, Object> rowMap) {
        SqlParams sqlParams = StaticSqlBuilder.getUpdateSql(modelName, rowMap);
        if (ModelManager.isVersionControl(modelName) && rowMap.containsKey(ModelConstant.VERSION)) {
            // Check the optimistic lock version control
            Object nextVersion = rowMap.get(ModelConstant.VERSION);
            Integer oldVersion = (Integer) nextVersion - 1;
            sqlParams.setSql(sqlParams.getSql() + " AND version = ?");
            sqlParams.addArgValue(oldVersion);
            int result = jdbcProxy.update(modelName, sqlParams);
            if (result == 0) {
                throw new VersionException("""
                        Data version does not match, may have been modified, please refresh and try again!
                        Provided value: {0}, database value: {1}, the two are not equal.""", oldVersion, nextVersion);
            }
            return result;
        } else {
            return jdbcProxy.update(modelName, sqlParams);
        }
    }

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
    @SkipPermissionCheck
    public Integer updateList(String modelName, List<Map<String, Object>> rows, Set<String> toUpdateFields) {
        LocalDateTime updatedTime = LocalDateTime.now();
        DataUpdatePipeline pipeline = new DataUpdatePipeline(modelName, toUpdateFields);
        // TODO: Process according to the `enableChangeLog` config, referring the TODO in ChangeLogPublisher.class
        //  if enableChangeLog = false, there is no need to get original data, compare differences and collect changeLogs.
        Map<Serializable, Map<String, Object>> originalRowsMap = this.getOriginalRowMap(modelName, rows, pipeline.getDifferFields());
        // Get the list of changed data, keeping only the fields and row data that have changed.
        List<Map<String, Object>> differRows = pipeline.processUpdateData(rows, originalRowsMap, updatedTime);
        int count = differRows.stream().mapToInt(row -> updateOne(modelName, row)).sum();
        // After updating the main table, update the sub-table to avoid the sub-table cascade field being the old value.
        boolean changed = pipeline.processXToManyData(rows);
        if (count > 0) {
            // Collect changeLogs
            changeLogPublisher.publishUpdateLog(modelName, differRows, originalRowsMap, updatedTime);
        } else if (changed) {
            // If the main table is not updated, but the sub-table is updated, mark the main table as updated
            return 1;
        }
        return count;
    }

    /**
     * Get the original data value.
     *
     * @param modelName Model name
     * @param rows Data list
     * @param differFields Fields that have changed
     * @return Map of original data
     */
    private Map<Serializable, Map<String, Object>> getOriginalRowMap(String modelName, List<Map<String, Object>> rows, Set<String> differFields) {
        // TODO: Extract to the upper layer, perform permission check in this method, and query the database one less time.
        // Get the original value
        String primaryKey = ModelManager.getModelPrimaryKey(modelName);
        List<K> pKeys = Cast.of(rows.stream().map(row -> row.get(primaryKey)).collect(Collectors.toList()));
        List<String> readFields = new ArrayList<>(differFields);
        List<Map<String, Object>> originalRows = this.selectByIds(modelName, pKeys, readFields, ConvertType.ORIGINAL);
        // Get the original data map with the primary key as the key
        return originalRows.stream().collect(Collectors.toMap(map -> (Serializable) map.get(primaryKey), Function.identity()));
    }

    /**
     * Deletes a single slice from a timeline model, identified by its slice ID.
     *
     * @param modelName the name of the model
     * @param sliceId the unique slice ID in a timeline model
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    public boolean deleteBySliceId(String modelName, Serializable sliceId) {
        // Get the original data before deletion for collecting changeLogs
        FlexQuery flexQuery = new FlexQuery(new Filters().eq(ModelConstant.SLICE_ID, sliceId)).acrossTimelineData();
        flexQuery.setConvertType(ConvertType.ORIGINAL);
        List<Map<String, Object>> originalRows = this.selectByFilter(modelName, flexQuery);
        // Physical deletion of timeline slice
        SqlParams sqlParams = StaticSqlBuilder.getDeleteTimelineSliceSql(modelName, sliceId);
        boolean result = jdbcProxy.update(modelName, sqlParams) > 0;
        // Collect changeLogs, changeLogs are bound to the database primary key
        changeLogPublisher.publishDeletionLog(modelName, originalRows, LocalDateTime.now());
        return result;
    }

    /**
     * Deletes multiple rows by their IDs. The {@code rows} parameter can be used
     * to send a change log or audit trail, if applicable.
     *
     * @param modelName the name of the model
     * @param ids a list of row IDs to delete
     * @param deletableRows the list of data corresponding to the rows to be deleted
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    public boolean deleteByIds(String modelName, List<K> ids, List<Map<String, Object>> deletableRows) {
        int count;
        LocalDateTime deleteTime = LocalDateTime.now();
        if (ModelManager.isSoftDeleted(modelName)) {
            // Soft delete data, assemble the update data list according to the ids list, and fill in the audit fields.
            List<Map<String, Object>> rows = ids.stream().map(id -> {
                Map<String, Object> row = new HashMap<>();
                row.put(ModelConstant.ID, id);
                row.put(ModelConstant.SOFT_DELETED_FIELD, true);
                return row;
            }).collect(Collectors.toList());
            AutofillFields.fillAuditFieldsForUpdate(rows, deleteTime);
            count = rows.stream().mapToInt(row -> updateOne(modelName, row)).sum();
        } else {
            // Delete data physically
            SqlParams sqlParams = StaticSqlBuilder.getDeleteSql(modelName, ids);
            count = jdbcProxy.update(modelName, sqlParams);
        }
        // Collect changeLogs
        changeLogPublisher.publishDeletionLog(modelName, deletableRows, deleteTime);
        return count > 0;
    }

    /**
     * Counts the number of rows matching the given {@link FlexQuery}.
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} defining filters, fields, etc.
     * @return the total count of matching rows
     */
    @SkipPermissionCheck
    public Long count(String modelName, FlexQuery flexQuery) {
        SqlParams sqlParams = SqlBuilderFactory.buildCountSql(modelName, flexQuery);
        return (Long) jdbcProxy.queryForObject(modelName, sqlParams, Long.class);
    }

    /**
     * Retrieves data as a list of entities of the specified class,
     * not relying on any metadata from the model manager.
     *
     * @param <T> the type of the entity
     * @param entityClass the target entity class
     * @param orderBy an optional "ORDER BY" clause
     * @return a list of entities
     */
    public <T> List<T> selectMetaEntityList(Class<T> entityClass, String orderBy) {
        String modelName = entityClass.getSimpleName();
        SqlParams sqlParams = StaticSqlBuilder.getSelectAllMetaSql(modelName, orderBy);
        List<Map<String, Object>> rows = jdbcProxy.queryForList(modelName, sqlParams);
        // Convert the original value of the database to an object
        return BeanTool.originalMapListToObjects(rows, entityClass, false);
    }

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
    public <T> List<T> selectMetaEntityList(String modelName, Class<T> entityClass, String orderBy) {
        SqlParams sqlParams = StaticSqlBuilder.getSelectAllMetaSql(modelName, orderBy);
        List<Map<String, Object>> rows = jdbcProxy.queryForList(modelName, sqlParams);
        // Convert the original value of the database to an object
        return BeanTool.originalMapListToObjects(rows, entityClass, true);
    }
}
