package info.openmeta.framework.orm.service.impl;

import com.google.common.collect.Sets;
import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.constant.StringConstant;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.exception.SecurityException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.Cast;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.*;
import info.openmeta.framework.orm.entity.TimelineSlice;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.jdbc.JdbcService;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.orm.service.PermissionService;
import info.openmeta.framework.orm.service.TimelineService;
import info.openmeta.framework.orm.utils.BeanTool;
import info.openmeta.framework.orm.utils.IdUtils;
import jakarta.validation.constraints.NotNull;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Common Model Service Interface.
 * The basic CRUD operations for the model.
 *
 * @param <K> id class
 */
@Slf4j
@Service
public class ModelServiceImpl<K extends Serializable> implements ModelService<K> {

    @Autowired
    private JdbcService<K> jdbcService;

    @Lazy
    @Autowired
    private PermissionService permissionService;

    @Autowired
    private TimelineService timelineService;

    private void checkTenantId(String modelName, List<Map<String, Object>> rows) {
        if (ModelManager.isMultiTenant(modelName)) {
            rows.forEach(row -> {
                if (row.containsKey(ModelConstant.TENANT_ID)) {
                    Long tenantId = (Long) row.get(ModelConstant.TENANT_ID);
                    if (tenantId != null && !tenantId.equals(ContextHolder.getContext().getTenantId())) {
                        throw new SecurityException("In a multi-tenancy environment, cross-tenant data access is not allowed: {0}", row);
                    }
                    row.remove(ModelConstant.TENANT_ID);
                }
            });
        }
    }

    /**
     * Create a single row and return the id.
     *
     * @param modelName model name
     * @param row data row to be created
     * @return id
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public K createOne(String modelName, Map<String, Object> row) {
        Assert.notEmpty(row, "The creation data for model {0} cannot be empty!", modelName);
        List<K> result = this.createList(modelName, Collections.singletonList(row));
        return result.getFirst();
    }

    /**
     * Create a single row, fetch the row data after creation.
     *
     * @param modelName model name
     * @param row data row to be created
     * @param convertType data convert type of the return value.
     * @return row data with id and other latest field values
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Map<String, Object> createOneAndFetch(String modelName, Map<String, Object> row, ConvertType convertType) {
        return this.createListAndFetch(modelName, Collections.singletonList(row), convertType).getFirst();
    }

    /**
     * Create multiple rows and return the id list.
     *
     * @param modelName model name
     * @param rows data rows to be created
     * @return id list
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<K> createList(String modelName, List<Map<String, Object>> rows) {
        Assert.allNotNull(rows, "The creation data for model {0} must not be empty: {1}", modelName, rows);
        this.checkTenantId(modelName, rows);
        // Extracts a set of assigned fields for checking field-level permissions
        Set<String> assignedFields = new HashSet<>();
        rows.forEach(row -> {
            ModelConstant.AUDIT_FIELDS.forEach(row::remove);
            assignedFields.addAll(row.keySet());
        });
        // The ids can only be extracted after jdbcService is called
        if (ModelManager.isTimelineModel(modelName)) {
            rows = timelineService.createSlices(modelName, rows);
        } else {
            rows = jdbcService.insertList(modelName, rows);
        }
        List<Serializable> ids = rows.stream().map(row -> (Serializable) row.get(ModelConstant.ID)).collect(Collectors.toList());
        // Keep only updatable fields
        assignedFields.retainAll(ModelManager.getModelUpdatableFields(modelName));
        // Checks ids and assigned fields access
        permissionService.checkIdsFieldsAccess(modelName, ids, assignedFields, AccessType.CREATE);
        return Cast.of(ids);
    }

    /**
     * Create multiple rows, fetch the rows after creation.
     *
     * @param modelName model name
     * @param rows data rows to be created
     * @param convertType data convert type of the return value.
     * @return row data list with id and other latest field values
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<Map<String, Object>> createListAndFetch(String modelName, List<Map<String, Object>> rows, ConvertType convertType) {
        List<K> ids = this.createList(modelName, rows);
        return this.getRowsWithoutPermissionCheck(modelName, ids, Collections.emptyList(), convertType);
    }

    /**
     * Get multiple rows by ids, without permission check, only for code use.
     *
     * @param modelName model name
     * @param ids List of data ids
     * @param fields Field list to read
     * @param convertType data convert type of the return value.
     * @return data list
     */
    private List<Map<String, Object>> getRowsWithoutPermissionCheck(String modelName, List<K> ids, List<String> fields, ConvertType convertType) {
        if (ModelManager.isTimelineModel(modelName)) {
            // Append timeline filters
            Filters filters = timelineService.appendTimelineFilters(modelName, new Filters().in(ModelConstant.ID, ids));
            FlexQuery flexQuery = new FlexQuery(fields, filters);
            flexQuery.setConvertType(convertType);
            return jdbcService.selectByFilter(modelName, flexQuery);
        }
        return jdbcService.selectByIds(modelName, ids, fields, convertType);
    }

    /**
     * Get one row by id, default to read all fields.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id data id
     * @return data row
     */
    @Override
    public Map<String, Object> getById(String modelName, K id) {
        return this.getById(modelName, id, Collections.emptyList(), null, ConvertType.TYPE_CAST);
    }

    /**
     * Get one row by id, default to read all fields.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id data id
     * @param subQueries SubQueries object, used to specify the subQuery for different relational fields.
     * @return data row
     */
    @Override
    public Map<String, Object> getById(String modelName, K id, SubQueries subQueries) {
        return this.getById(modelName, id, Collections.emptyList(), subQueries, ConvertType.TYPE_CAST);
    }

    /**
     * Get one row by id.
     * If the fields is not specified, all accessible fields as the default.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id data id
     * @param fields field list to get value
     * @return data row
     */
    @Override
    public Map<String, Object> getById(String modelName, K id, Collection<String> fields) {
        return this.getById(modelName, id, fields, null, ConvertType.TYPE_CAST);
    }

    /**
     * Get one row by id.
     * If the fields is not specified, all accessible fields as the default.
     *
     * @param id data id
     * @param fields field list to get value
     * @param subQueries SubQueries object, used to specify the subQuery for different relational fields.
     * @param convertType data convert type of the return value.
     * @return data row
     */
    @Override
    public Map<String, Object> getById(String modelName, K id, Collection<String> fields,
                                       SubQueries subQueries, ConvertType convertType) {
        List<Map<String, Object>> rows = this.getByIds(modelName, Collections.singletonList(id), fields, subQueries, convertType);
        Assert.notEmpty(rows, "Model {0} does not have data with id {1}!", modelName, id);
        return rows.getFirst();
    }

    /**
     * Get multiple rows by ids.
     * If the fields is not specified, all accessible fields as the default.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param ids List of data ids
     * @param fields Field list to get value
     * @return List<Map> of multiple data
     */
    @Override
    public List<Map<String, Object>> getByIds(String modelName, List<K> ids, Collection<String> fields) {
        return this.getByIds(modelName, ids, fields, null, ConvertType.TYPE_CAST);
    }

    /**
     * Get multiple rows by ids.
     * If the fields is not specified, all accessible fields as the default.
     *
     * @param ids List of data ids
     * @param fields Field list to get value
     * @param subQueries SubQueries object, used to specify the subQuery for different relational fields.
     * @param convertType data convert type of the return value.
     * @return List<Map> of multiple data
     */
    @Override
    public List<Map<String, Object>> getByIds(String modelName, @NotNull List<K> ids, Collection<String> fields,
                                              SubQueries subQueries, ConvertType convertType) {
        ids = ids.stream().filter(IdUtils::validId).collect(Collectors.toList());
        if (ids.isEmpty()) {
            return Collections.emptyList();
        }
        FlexQuery flexQuery = new FlexQuery(fields, Filters.of(ModelConstant.ID, Operator.IN, ids));
        flexQuery.setConvertType(convertType);
        List<Map<String, Object>> rows = this.searchList(modelName, flexQuery);
        List<Serializable> resultIds = rows.stream().map(row -> (Serializable) row.get(ModelConstant.ID)).toList();
        if (ids.size() > resultIds.size()) {
            permissionService.checkIdsAccess(modelName, ids, AccessType.READ);
        }
        return rows;
    }

    /**
     * Get the copyable fields value by ID, no data inserted.
     *
     * @param modelName model name
     * @param id source data id
     * @return map of copyable field values
     */
    @Override
    public Map<String, Object> getCopyableFields(String modelName, K id) {
        Map<String, Object> value = this.getById(modelName, id, Collections.emptyList());
        List<String> copyableFields = ModelManager.getModelCopyableFields(modelName);
        copyableFields.forEach(value::remove);
        return value;
    }

    /**
     * Get the `displayNames` of the specified ids, returning map of {id: displayName}.
     * If the `displayFields` is not specified, use the `displayName` configuration of the model.
     *
     * @param modelName model name
     * @param ids id list
     * @param displayFields specified display field list.
     * @return displayNames Map
     */
    @Override
    public Map<K, String> getDisplayNames(String modelName, List<K> ids, List<String> displayFields) {
        if (CollectionUtils.isEmpty(ids)) {
            return new HashMap<>(0);
        }
        if (CollectionUtils.isEmpty(displayFields)) {
            displayFields = ModelManager.getModel(modelName).getDisplayName();
        }
        Set<String> getFields = new HashSet<>(displayFields);
        getFields.add(ModelConstant.ID);
        Filters filters = Filters.of(ModelConstant.ID, Operator.IN, ids);
        FlexQuery flexQuery = new FlexQuery(getFields, filters);
        // If the `displayName` config consists of an Option field or a ManyToOne/OneToOne field,
        // get the optionItemName or cascaded displayName value as its field value.
        flexQuery.setConvertType(ConvertType.DISPLAY);
        List<Map<String, Object>> rows = this.searchList(modelName, flexQuery);
        Map<K, String> displayNames = new HashMap<>();
        for (Map<String, Object> row : rows) {
            // Filter out field values for null or empty strings
            List<Object> displayValues = displayFields.stream().map(row::get).filter(v -> v != null && v != "").collect(Collectors.toList());
            String name = StringUtils.join(displayValues, StringConstant.DISPLAY_NAME_SEPARATOR);
            displayNames.put(Cast.of(row.get(ModelConstant.ID)), name);
        }
        return displayNames;
    }

    /**
     * Get the distinct field value list based on the filters.
     *
     * @param modelName model name
     * @param field field name to read and distinct
     * @param filters filters
     * @return distinct field value list
     */
    @Override
    public List<Object> getDistinctFieldValue(String modelName, String field, Filters filters) {
        FlexQuery flexQuery = new FlexQuery(Sets.newHashSet(field), filters);
        List<Map<String, Object>> rows = searchList(modelName, flexQuery);
        return rows.stream().map(r -> r.get(field)).distinct().collect(Collectors.toList());
    }

    /**
     * Get the specified field value based on id.
     * The ManyToOne/OneToOne/Option/MultiOption fields are original values.
     *
     * @param id data id
     * @param field field name to get value
     * @return field value
     */
    @Override
    public <V extends Serializable> V getFieldValue(String modelName, K id, String field) {
        Map<String, Object> row = this.getById(modelName, id, Collections.singletonList(field), null, ConvertType.TYPE_CAST);
        return Cast.of(row.get(field));
    }

    /**
     * Get the ids based on the filters.
     *
     * @param modelName model name
     * @param filters filter conditions
     * @return ids list
     */
    @Override
    public List<K> getIds(String modelName, Filters filters) {
        // Append timeline filtersAppend timeline filters
        filters = timelineService.appendTimelineFilters(modelName, filters);
        // Append permission data range filters
        filters = permissionService.appendScopeAccessFilters(modelName, filters);
        FlexQuery flexQuery = new FlexQuery(filters);
        return jdbcService.getIds(modelName, ModelConstant.ID, flexQuery);
    }

    /**
     * Get the externalId-id mapping based on the externalIds.
     *
     * @param modelName model name
     * @param externalIds externalId list
     * @return externalId-id mapping
     */
    private Map<Serializable, K> getExternalIdMapping(String modelName, List<Serializable> externalIds) {
        List<String> fields = Arrays.asList(ModelConstant.ID, ModelConstant.EXTERNAL_ID);
        Filters filters = new Filters().in(ModelConstant.EXTERNAL_ID, externalIds);
        FlexQuery flexQuery = new FlexQuery(fields, filters);
        List<Map<String, Object>> rows = this.searchList(modelName, flexQuery);
        return rows.stream().collect(Collectors.toMap(
                row -> (Serializable) row.get(ModelConstant.EXTERNAL_ID),
                row -> Cast.of(row.get(ModelConstant.ID))));
    }

    /**
     * Get the ids for ManyToOne/OneToOne relational field.
     *
     * @param modelName model name
     * @param filters filters
     * @param fieldName relational field name
     * @return distinct ids for relational field
     */
    @Override
    public <EK extends Serializable> List<EK> getRelatedIds(String modelName, Filters filters, String fieldName) {
        // Append timeline filtersAppend timeline filters
        filters = timelineService.appendTimelineFilters(modelName, filters);
        // Append permission data range filters
        filters = permissionService.appendScopeAccessFilters(modelName, filters);
        FlexQuery flexQuery = new FlexQuery(filters);
        // Automatic distinct when querying relational field ids
        flexQuery.setDistinct(true);
        List<EK> relatedIds = jdbcService.getIds(modelName, fieldName, flexQuery);
        // Filter out null value
        return relatedIds.stream().filter(IdUtils::validId).collect(Collectors.toList());
    }

    /**
     * Get the unmasked field value based on the id and field name.
     *
     * @param modelName model name
     * @param id data id
     * @param fieldName masking field name
     * @return unmasked field value
     */
    @Override
    public String getUnmaskedField(String modelName, K id, String fieldName) {
        return (String) this.getUnmaskedFields(modelName, id, Collections.singletonList(fieldName)).get(fieldName);
    }

    /**
     * Get multiple unmasked field values based on the id and field names.
     *
     * @param modelName model name
     * @param id data id
     * @param fields masking field names
     * @return unmasked field values map, {fieldName: fieldValue}
     */
    @Override
    public Map<String, Object> getUnmaskedFields(String modelName, K id, List<String> fields) {
        fields.forEach(field -> {
            MetaField metaField = ModelManager.getModelField(modelName, field);
            Assert.notNull(metaField.getMaskingType(),
                    "The `maskingType` of model {0} field {1} is null, cannot invoke unmask!",
                    modelName, field);
        });
        return getById(modelName, id, fields);
    }

    /**
     * Update one row by id.
     *
     * @param value data row to be updated
     * @return true / Exception
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean updateOne(String modelName, Map<String, Object> value) {
        return this.updateList(modelName, Collections.singletonList(value));
    }

    /**
     * Update one row by id, and fetch the updated row from database.
     *
     * @param modelName model name
     * @param value data row to be updated
     * @param convertType data convert type of the return value.
     * @return updated row with the latest field values
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Map<String, Object> updateOneAndFetch(String modelName, Map<String, Object> value, ConvertType convertType) {
        return this.updateListAndFetch(modelName, Collections.singletonList(value), convertType).getFirst();
    }

    /**
     * Update multiple rows by id.
     * Each row in the list can have different fields.
     *
     * @param rows data rows to be updated
     * @return true / Exception
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean updateList(String modelName, List<Map<String, Object>> rows) {
        Set<String> updatableFields = ModelManager.getModelUpdatableFields(modelName);
        Set<String> toUpdateFields = new HashSet<>();
        List<Map<String, Object>> toUpdateRows = new ArrayList<>();
        String pk = ModelManager.getModelPrimaryKey(modelName);
        rows.forEach(row -> {
            // Remove audit fields from the update data
            ModelConstant.AUDIT_FIELDS.forEach(row::remove);
            Set<String> rowFields = new HashSet<>(row.keySet());
            rowFields.retainAll(updatableFields);
            Assert.notNull(row.get(pk),
                    "When updating model {0}, the primary key {1} cannot be null! {2}", modelName, pk, row);
            if (rowFields.isEmpty()) {
                log.warn("Missing business fields when updating model {} data! {}, automatically ignored.", modelName, row);
                return;
            }
            // Only keep the updatable fields, and format and store the data for these fields
            toUpdateFields.addAll(rowFields);
            toUpdateRows.add(row);
        });
        if (toUpdateRows.isEmpty()) {
            log.warn("Model {} has no data to update and the request has been ignored. Check if updatable fields are included. {}.", modelName, rows);
            return false;
        }
        this.checkTenantId(modelName, rows);
        List<Serializable> pks = rows.stream().map(row -> (Serializable) row.get(pk)).collect(Collectors.toList());
        Integer updateCount;
        if (ModelManager.isTimelineModel(modelName)) {
            FlexQuery flexQuery = new FlexQuery(Arrays.asList(ModelConstant.ID, ModelConstant.SLICE_ID), new Filters().in(ModelConstant.SLICE_ID, pks));
            List<Map<String, Object>> sliceList = jdbcService.selectByFilter(modelName, flexQuery);
            Map<Serializable, Serializable> sliceMap = sliceList.stream().collect(Collectors.toMap(row -> (Serializable) row.get(ModelConstant.SLICE_ID), row -> (Serializable) row.get(ModelConstant.ID)));
            // Fill timeline model business primary key `id`. The input id parameter is not reliable.
            rows.forEach(row -> {
                Serializable sliceId = (Serializable) row.get(ModelConstant.SLICE_ID);
                Assert.isTrue(sliceMap.containsKey(sliceId),
                        "The timeline model {0} does not have data for sliceId {1}!", modelName, sliceId);
                row.put(ModelConstant.ID, sliceMap.get(sliceId));
            });
            permissionService.checkIdsFieldsAccess(modelName, sliceMap.values(), toUpdateFields, AccessType.UPDATE);
            updateCount = timelineService.updateSlices(modelName, rows);
        } else {
            permissionService.checkIdsFieldsAccess(modelName, pks, toUpdateFields, AccessType.UPDATE);
            updateCount = jdbcService.updateList(modelName, rows, toUpdateFields);
        }
        return updateCount > 0;
    }

    /**
     * Update multiple rows by ids, and fetch the updated rows from database.
     * Each row in the list can have different fields.
     *
     * @param modelName model name
     * @param rows data rows to be updated
     * @param convertType data convert type of the return value.
     * @return updated rows with the latest field values
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<Map<String, Object>> updateListAndFetch(String modelName, List<Map<String, Object>> rows, ConvertType convertType) {
        this.updateList(modelName, rows);
        List<K> ids = Cast.of(rows.stream().map(r -> r.get(ModelConstant.ID)).collect(Collectors.toList()));
        return this.getRowsWithoutPermissionCheck(modelName, ids, Collections.emptyList(), convertType);
    }

    /**
     * Update multiple rows by externalId. Each row in the list can have different fields.
     *
     * @param rows data rows to be updated
     * @return true / Exception
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean updateByExternalId(String modelName, List<Map<String, Object>> rows) {
        List<Serializable> externalIds = rows.stream()
                .map(row -> {
                    Serializable externalId = (Serializable) row.get(ModelConstant.EXTERNAL_ID);
                    Assert.notNull(externalId, "The externalId field must be included in the update data! {0}", row);
                    return externalId;
                }).toList();
        Map<Serializable, K> eIdMap = this.getExternalIdMapping(modelName, externalIds);
        List<Serializable> differenceIds = externalIds.stream().filter(externalId -> !eIdMap.containsKey(externalId)).toList();
        Assert.isEmpty(differenceIds, "The externalId {0} does not exist in model {1}!", differenceIds, modelName);
        // Fill the id field with the value obtained by externalId
        rows.forEach(row -> row.put(ModelConstant.ID, eIdMap.get((Serializable) row.get(ModelConstant.EXTERNAL_ID))));
        return this.updateList(modelName, rows);
    }

    /**
     * Batch edit data based on the filters, according to the specified field values map.
     *
     * @param filters filters, if not specified, all visible data of the current user will be updated.
     * @param value field values to be updated
     * @return number of affected rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer updateByFilter(String modelName, Filters filters, Map<String, Object> value) {
        Assert.notTrue(value.containsKey(ModelConstant.ID),
                "When batch editing data through Filters, `value` parameter cannot contains the key of `id`!");
        List<K> ids = this.getIds(modelName, filters);
        for (int i = 0; i < ids.size(); i += BaseConstant.DEFAULT_BATCH_SIZE) {
            List<K> idsBatch = ids.subList(i, Math.min(i + BaseConstant.DEFAULT_BATCH_SIZE, ids.size()));
            List<Map<String, Object>> rowBatch = idsBatch.stream().map(id -> {
                Map<String, Object> newRow = new HashMap<>(value);
                newRow.put(ModelConstant.ID, id);
                return newRow;
            }).collect(Collectors.toList());
            updateList(modelName, rowBatch);
        }
        return null;
    }

    /**
     * Delete one row by id.
     * All the slices related to this `id` will be deleted if the model is a timeline model.
     *
     * @param id data id
     * @return true / Exception
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean deleteById(String modelName, K id) {
        return this.deleteByIds(modelName, Collections.singletonList(id));
    }

    /**
     * Delete a slice of timeline model by `sliceId`, the primary key of timeline model.
     *
     * @param modelName model name
     * @param sliceId data id
     * @return true / Exception
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean deleteBySliceId(String modelName, Long sliceId) {
        Assert.isTrue(ModelManager.isTimelineModel(modelName),
                "Model {0} is not a timeline model, and cannot delete slice.", modelName);
        TimelineSlice timelineSlice = timelineService.getTimelineSlice(modelName, sliceId);
        permissionService.checkIdsAccess(modelName, Collections.singletonList(timelineSlice.getId()), AccessType.DELETE);
        return timelineService.deleteSlice(modelName, timelineSlice);
    }

    /**
     * Delete multiple rows by ids.
     *
     * @param ids data ids
     * @return true / Exception
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean deleteByIds(String modelName, List<K> ids) {
        Assert.allNotNull(ids, "The ids to be deleted cannot be empty! {0}", ids);
        permissionService.checkIdsAccess(modelName, ids, AccessType.DELETE);
        // Get the pre-delete data, to check whether the ids data have been deleted and collect changeLogs.
        List<Map<String, Object>> originalRows = jdbcService.selectByIds(modelName, ids, Collections.emptyList(), ConvertType.ORIGINAL);
        List<Map<String, Object>> deletableRows = originalRows.stream().filter(row -> {
            if (ModelManager.isSoftDeleted(modelName) && Boolean.TRUE.equals(row.get(ModelConstant.SOFT_DELETED_FIELD))) {
                log.warn("Data with model {}, id={} has been soft deleted, do not duplicate it.", modelName, row.get(ModelConstant.ID));
                return false;
            }
            return true;
        }).collect(Collectors.toList());
        List<Serializable> deletableIds = deletableRows.stream().map(m -> (Serializable) m.get(ModelConstant.ID)).toList();
        if (CollectionUtils.isEmpty(deletableIds)) {
            return false;
        }
        return jdbcService.deleteByIds(modelName, Cast.of(deletableIds), deletableRows);
    }

    /**
     * Delete multiple rows by externalIds.
     *
     * @param externalIds externalId List
     * @return true / Exception
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean deleteByExternalIds(String modelName, List<Serializable> externalIds) {
        Assert.allNotNull(externalIds, "The externalIds to be deleted cannot be empty! {0}", externalIds);
        Map<Serializable, K> eIdMap = this.getExternalIdMapping(modelName, externalIds);
        List<Serializable> differenceIds = externalIds.stream().filter(externalId -> !eIdMap.containsKey(externalId)).toList();
        Assert.isEmpty(differenceIds, "The externalId {0} does not exist in model {1}!", differenceIds, modelName);
        return this.deleteByIds(modelName, new ArrayList<>(eIdMap.values()));
    }

    /**
     * Delete rows by specified filters.
     *
     * @param modelName model name
     * @param filters filters
     * @return true / Exception
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean deleteByFilters(String modelName, Filters filters) {
        List<K> ids = this.getIds(modelName, filters);
        if (CollectionUtils.isEmpty(ids)) {
            return false;
        }
        return this.deleteByIds(modelName, ids);
    }

    /**
     * Copy a single row based on id, and return the new id of the new row.
     * Currently, read and create permissions are checked separately
     *
     * @param modelName model name
     * @param id source data id
     * @return id of the new data
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public K copyById(String modelName, K id) {
        return this.copyByIds(modelName, Collections.singletonList(id)).getFirst();
    }

    /**
     * Copy a single row based on id, and fetch the new row.
     * Currently, read and create permissions are checked separately
     *
     * @param modelName model name
     * @param id source data id
     * @param convertType data convert type of the return value.
     * @return new row data
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Map<String, Object> copyByIdAndFetch(String modelName, K id, ConvertType convertType) {
        return this.copyByIdsAndFetch(modelName, Collections.singletonList(id), convertType).getFirst();
    }

    /**
     * Copy multiple rows based on ids, and return the new ids of the new rows.
     * Currently, read and create permissions are checked separately
     *
     * @param modelName model name
     * @param ids source data ids
     * @return ids of the created data
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<K> copyByIds(String modelName, List<K> ids) {
        List<Map<String, Object>> rows = this.getByIds(modelName, ids, null);
        List<String> copyableFields = ModelManager.getModelCopyableFields(modelName);
        rows.forEach(row -> copyableFields.forEach(row::remove));
        this.createList(modelName, rows);
        return Cast.of(rows.stream().map(row -> row.get(ModelConstant.ID)).collect(Collectors.toList()));
    }

    /**
     * Copy multiple rows based on ids, and fetch the new rows.
     * Currently, read and create permissions are checked separately
     *
     * @param modelName model name
     * @param ids source data ids
     * @param convertType data convert type of the return value.
     * @return new row data list
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<Map<String, Object>> copyByIdsAndFetch(String modelName, List<K> ids, ConvertType convertType) {
        List<K> newIds = this.copyByIds(modelName, ids);
        return this.getRowsWithoutPermissionCheck(modelName, newIds, Collections.emptyList(), convertType);
    }

    /**
     * Query single row data based on filters. Only for code use.
     * Throw an exception when there are multiple objects.
     *
     * @param modelName model name
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @return single row data
     */
    @Override
    public Map<String, Object> searchOne(String modelName, FlexQuery flexQuery) {
        List<Map<String, Object>> rows = this.searchList(modelName, flexQuery);
        if (rows.isEmpty()) {
            return Collections.emptyMap();
        } else if (rows.size() > 1) {
            throw new IllegalArgumentException("The number of `searchOne` results for model {0} is greater than 1: {1}",
                    modelName, flexQuery);
        }
        return rows.getFirst();
    }

    /**
     * Query data list based on FlexQuery without pagination, only for code use.
     * If the result exceeds the MAX_BATCH_SIZE, an error log is recorded, but no exception is thrown.
     *
     * @param modelName model name
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @return data list
     */
    @Override
    public List<Map<String, Object>> searchList(String modelName, FlexQuery flexQuery) {
        // Check model and field level permissions
        permissionService.checkModelFieldsAccess(modelName, flexQuery.getFields(), AccessType.READ);
        // Append timeline filters
        Filters filters = timelineService.appendTimelineFilters(modelName, flexQuery);
        // Append permission data range filters
        filters = permissionService.appendScopeAccessFilters(modelName, filters);
        flexQuery.setFilters(filters);
        List<Map<String, Object>> result = jdbcService.selectByFilter(modelName, flexQuery);
        if (result.size() > BaseConstant.MAX_BATCH_SIZE) {
            log.error("Model {} `searchList` exceeds the limit of {}, please switch to `searchPage`: {}",
                    modelName, BaseConstant.MAX_BATCH_SIZE, flexQuery);
        }
        return result;
    }

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
    @Override
    public <R> List<R> searchList(String modelName, FlexQuery flexQuery, Class<R> returnClass) {
        List<Map<String, Object>> rows = this.searchList(modelName, flexQuery);
        return CollectionUtils.isEmpty(rows) ? Collections.emptyList() : BeanTool.mapListToObjects(rows, returnClass);
    }

    /**
     * Query data list based on FlexQuery with pagination.
     * The page size cannot exceed the MAX_BATCH_SIZE.
     *
     * @param modelName model name
     * @param flexQuery FlexQuery object, can set fields, filters, orders, etc.
     * @param page the Page object containing pagination information
     * @return a Page object containing the Map data list
     */
    @Override
    public Page<Map<String, Object>> searchPage(String modelName, FlexQuery flexQuery, Page<Map<String, Object>> page) {
        // Check model and field level permissions
        permissionService.checkModelFieldsAccess(modelName, flexQuery.getFields(), AccessType.READ);
        // Append timeline filters
        Filters filters = timelineService.appendTimelineFilters(modelName, flexQuery);
        // Append permission data range filters
        filters = permissionService.appendScopeAccessFilters(modelName, filters);
        flexQuery.setFilters(filters);
        return jdbcService.selectByPage(modelName, flexQuery, page);
    }

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
    @Override
    public <R> Page<R> searchPage(String modelName, FlexQuery flexQuery, Page<R> page, Class<R> returnClass) {
        Page<Map<String, Object>> mapPage = new Page<>(page.getPageNumber(), page.getPageSize(), page.isCursorPage(), page.isCount());
        this.searchPage(modelName, flexQuery, mapPage);
        if (!CollectionUtils.isEmpty(mapPage.getRows())) {
            List<R> objects = BeanTool.mapListToObjects(mapPage.getRows(), returnClass);
            page.setRows(objects);
            page.setTotal(mapPage.getTotal());
        }
        return page;
    }

    /**
     * Return data in a tree structure.
     *
     * @param flexQuery search conditions, can set fields, filters, orders, etc.
     * @return Tree structure data list
     */
    @Override
    public List<Map<String, Object>> searchTree(String modelName, FlexQuery flexQuery) {
        return Collections.emptyList();
    }

    /**
     * Query the PivotTable, without pagination, the data is limited to no more than 10000 records.
     *
     * @param flexQuery search conditions, can set fields, filters, orders, groupBy, splitBy, summary, etc.
     * @return PivotTable object
     */
    @Override
    public PivotTable searchPivot(String modelName, FlexQuery flexQuery) {
        List<Map<String, Object>> rows = this.searchList(modelName, flexQuery);
        // To perform aggregate calculation for numeric fields, remove the fields that appear in
        // `groupBy` and `splitBy` parameters to avoid aggregate calculation of the group fields.
        Set<String> numericFields = ModelManager.getModelNumericFields(modelName);
        numericFields.removeAll(new HashSet<>(flexQuery.getGroupBy()));
        numericFields.removeAll(new HashSet<>(flexQuery.getSplitBy()));
        Map<String, FieldType> numericFieldsType = numericFields.stream().map(field -> ModelManager.getModelField(modelName, field))
                .collect(Collectors.toMap(MetaField::getFieldName, MetaField::getFieldType));
        // Total the 'count' calculation as part of the PivotTable aggregation calculation.
        numericFieldsType.put("count", FieldType.LONG);
        // Aggregate operations into PivotTable structures
        return PivotTable.aggregateOperation(rows, flexQuery.getGroupBy(), flexQuery.getSplitBy(), numericFieldsType);
    }

    /**
     * Count the number of rows based on the filter conditions.
     *
     * @param modelName model name
     * @param filters filter conditions
     * @return count result
     */
    @Override
    public Long count(String modelName, Filters filters) {
        // Append timeline filters
        filters = timelineService.appendTimelineFilters(modelName, filters);
        // Append permission data range filters
        filters = permissionService.appendScopeAccessFilters(modelName, filters);
        return jdbcService.count(modelName, new FlexQuery(filters));
    }

    /**
     * Determine if the id physically exists, without permission check.
     *
     * @param modelName Model name
     * @param id Data id
     * @return true or false
     */
    @Override
    public boolean exist(String modelName, K id) {
        return jdbcService.exist(modelName, id);
    }

    /**
     * Filter the set that exist in the database from ids, without permission check.
     *
     * @param modelName Model name
     * @param ids Data ids
     * @return ids that exist in the database
     */
    @Override
    public List<K> filterExistIds(String modelName, Collection<K> ids) {
        FlexQuery flexQuery = new FlexQuery(new Filters().in(ModelConstant.ID, ids));
        return jdbcService.getIds(modelName, ModelConstant.ID, flexQuery);
    }

}
