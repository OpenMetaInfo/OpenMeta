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
                    String tenantId = (String) row.get(ModelConstant.TENANT_ID);
                    if (StringUtils.isNotBlank(tenantId) && !tenantId.equals(ContextHolder.getContext().getTenantId())) {
                        throw new SecurityException("In a multi-tenancy environment, cross-tenant data access is not allowed: {0}", row);
                    }
                    row.remove(ModelConstant.TENANT_ID);
                }
            });
        }
    }

    /**
     * Creates a new row in the specified model and returns its ID.
     *
     * @param modelName the name of the model
     * @param row the data to create
     * @return the ID of the newly created row
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public K createOne(String modelName, Map<String, Object> row) {
        Assert.notEmpty(row, "The creation data for model {0} cannot be empty!", modelName);
        List<K> result = this.createList(modelName, Collections.singletonList(row));
        return result.getFirst();
    }

    /**
     * Creates a new row in the specified model and returns the created data,
     * including any auto-generated fields, with the specified convert type.
     *
     * @param modelName the name of the model
     * @param row the data to create
     * @param convertType the conversion type applied to the result
     * @return a map containing the newly created row with updated field values
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Map<String, Object> createOneAndFetch(String modelName, Map<String, Object> row, ConvertType convertType) {
        return this.createListAndFetch(modelName, Collections.singletonList(row), convertType).getFirst();
    }

    /**
     * Creates multiple rows and returns a list of their IDs.
     *
     * @param modelName the name of the model
     * @param rows the list of data rows to create
     * @return a list of IDs corresponding to the newly created rows
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
        List<Serializable> ids = rows.stream()
                .map(row -> (Serializable) row.get(ModelConstant.ID))
                .collect(Collectors.toList());
        // Keep only updatable fields
        assignedFields.retainAll(ModelManager.getModelUpdatableFields(modelName));
        // Checks ids and assigned fields access
        permissionService.checkIdsFieldsAccess(modelName, ids, assignedFields, AccessType.CREATE);
        return Cast.of(ids);
    }

    /**
     * Creates multiple rows, returning the newly created data with any
     * auto-generated fields, and applying the specified convert type.
     *
     * @param modelName the name of the model
     * @param rows the list of data rows to create
     * @param convertType the conversion type applied to the results
     * @return a list of maps containing the newly created rows and their field values
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<Map<String, Object>> createListAndFetch(String modelName, List<Map<String, Object>> rows, ConvertType convertType) {
        List<K> ids = this.createList(modelName, rows);
        return this.getRowsWithoutPermissionCheck(modelName, ids, Collections.emptyList(), convertType);
    }

    /**
     * Get multiple rows by IDs, without permission check, only for code use.
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
     * Get a row by its ID. By default, all accessible fields are returned.
     * ManyToOne, OneToOne, Option, and MultiOption fields remain in their original form.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @return an {@link Optional} containing the row if found; otherwise empty
     */
    @Override
    public Optional<Map<String, Object>> getById(String modelName, K id) {
        return this.getById(modelName, id, Collections.emptyList(), null, ConvertType.TYPE_CAST);
    }

    /**
     * Get a row by its ID with all fields by default, optionally expanding relational fields via subQueries.
     * ManyToOne, OneToOne, Option, and MultiOption fields remain in their original form.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @param subQueries a {@link SubQueries} object describing how to expand relational fields
     * @return an {@link Optional} containing the row if found; otherwise empty
     */
    @Override
    public Optional<Map<String, Object>> getById(String modelName, K id, SubQueries subQueries) {
        return this.getById(modelName, id, Collections.emptyList(), subQueries, ConvertType.TYPE_CAST);
    }

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
    @Override
    public Optional<Map<String, Object>> getById(String modelName, K id, Collection<String> fields) {
        return this.getById(modelName, id, fields, null, ConvertType.TYPE_CAST);
    }

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
    @Override
    public Optional<Map<String, Object>> getById(String modelName, K id, Collection<String> fields,
                                       SubQueries subQueries, ConvertType convertType) {
        List<Map<String, Object>> rows = this.getByIds(modelName, Collections.singletonList(id), fields, subQueries, convertType);
        return rows.isEmpty() ? Optional.empty() : Optional.of(rows.getFirst());
    }

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
    @Override
    public List<Map<String, Object>> getByIds(String modelName, List<K> ids, Collection<String> fields) {
        return this.getByIds(modelName, ids, fields, null, ConvertType.TYPE_CAST);
    }

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
    @Override
    public List<Map<String, Object>> getByIds(String modelName, @NotNull List<K> ids, Collection<String> fields,
                                              SubQueries subQueries, ConvertType convertType) {
        ids = ids.stream().filter(IdUtils::validId).collect(Collectors.toList());
        if (ids.isEmpty()) {
            return Collections.emptyList();
        }
        FlexQuery flexQuery = new FlexQuery(fields, Filters.of(ModelConstant.ID, Operator.IN, ids));
        flexQuery.setSubQueries(subQueries);
        flexQuery.setConvertType(convertType);
        List<Map<String, Object>> rows = this.searchList(modelName, flexQuery);
        List<Serializable> resultIds = rows.stream().map(row -> (Serializable) row.get(ModelConstant.ID)).toList();
        if (ids.size() > resultIds.size()) {
            permissionService.checkIdsAccess(modelName, ids, AccessType.READ);
        }
        return rows;
    }

    /**
     * Get all copyable fields from a row, identified by its ID,
     * without inserting any new data.
     *
     * @param modelName the name of the model
     * @param id the ID of the source row
     * @return a map containing values of all copyable fields
     */
    @Override
    public Map<String, Object> getCopyableFields(String modelName, K id) {
        Map<String, Object> value = this.getById(modelName, id, Collections.emptyList())
                .orElseThrow(() -> new IllegalArgumentException("The data of model {0} with id {1} does not exist!", modelName, id));
        List<String> copyableFields = ModelManager.getModelCopyableFields(modelName);
        copyableFields.forEach(value::remove);
        return value;
    }

    /**
     * Get display names for the specified IDs, returning a map of {id -> displayName}.
     *
     * @param modelName the name of the model
     * @param ids a list of row IDs
     * @return a map of IDs to their resolved display names
     */
    @Override
    public Map<K, String> getDisplayNames(String modelName, List<K> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return new HashMap<>(0);
        }
        Filters filters = Filters.of(ModelConstant.ID, Operator.IN, ids);
        return this.getDisplayNames(modelName, filters);
    }

    /**
     * Get display names for the specified filters, returning a map of {id -> displayName}.
     *
     * @param modelName the name of the model
     * @param filters   the filters to apply
     * @return a map of IDs to their resolved display names
     */
    @Override
    public Map<K, String> getDisplayNames(String modelName, Filters filters) {
        List<String> displayFields = ModelManager.getModelDisplayName(modelName);
        Set<String> getFields = new HashSet<>(displayFields);
        FlexQuery flexQuery = new FlexQuery(getFields, filters);
        // If the `displayName` config consists of an Option field or a ManyToOne/OneToOne field,
        // get the optionItemName or cascaded displayName value as its field value.
        flexQuery.setConvertType(ConvertType.DISPLAY);
        List<Map<String, Object>> rows = this.searchList(modelName, flexQuery);
        Map<K, String> displayNames = new HashMap<>();
        for (Map<String, Object> row : rows) {
            // Filter out field values for null or empty strings
            List<Object> displayValues = displayFields.stream()
                    .map(row::get).filter(v -> v != null && v != "")
                    .toList();
            String name = StringUtils.join(displayValues, StringConstant.DISPLAY_NAME_SEPARATOR);
            displayNames.put(Cast.of(row.get(ModelConstant.ID)), name);
        }
        return displayNames;
    }

    /**
     * Get distinct values for the specified field, filtered by the given conditions.
     *
     * @param <V> the type of the field's value
     * @param modelName the name of the model
     * @param field the field name for which to retrieve distinct values
     * @param filters optional filtering conditions
     * @return a list of distinct field values
     */
    @Override
    public <V extends Serializable> List<V> getDistinctFieldValue(String modelName, String field, Filters filters) {
        FlexQuery flexQuery = new FlexQuery(Sets.newHashSet(field), filters);
        List<Map<String, Object>> rows = searchList(modelName, flexQuery);
        return rows.stream().map(r -> (V) r.get(field)).distinct().collect(Collectors.toList());
    }

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
    @Override
    public <V extends Serializable> V getFieldValue(String modelName, K id, String field) {
        Optional<Map<String, Object>> optionalRow = this.getById(modelName, id, Collections.singletonList(field));
        Object value = optionalRow.map(row -> row.get(field)).orElse(null);
        return Cast.of(value);
    }

    /**
     * Get a list of IDs matching the given filters.
     *
     * @param modelName the name of the model
     * @param filters filtering conditions
     * @return a list of matching IDs
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
     * Get ID by businessKey.
     * @param modelName model name
     * @param row data row
     * @return id
     */
    private K getIdByBusinessKey(String modelName, Map<String, Object> row) {
        List<String> businessKey = ModelManager.getModel(modelName).getBusinessKey();
        Assert.notEmpty(businessKey, "Model {0} does not have a business key, cannot get id by business key!", modelName);
        Filters filters = new Filters();
        businessKey.forEach(key -> filters.and(key, Operator.EQUAL, row.get(key)));
        List<K> ids = this.getIds(modelName, filters);
        if (CollectionUtils.isEmpty(ids)) {
            throw new IllegalArgumentException("Model {0} does not have data with the business key {1}!", modelName, row);
        } else if (ids.size() > 1) {
            throw new IllegalArgumentException("Model {0} has multiple data with the same business key {1}!", modelName, row);
        }
        return ids.getFirst();
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
     * Get a distinct list of IDs for a ManyToOne/OneToOne relationship field
     * based on the given filters.
     *
     * @param <EK> the type of the related entity's ID
     * @param modelName the name of the model
     * @param filters filtering conditions
     * @param fieldName the name of the relationship field
     * @return a distinct list of related entity IDs
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
     * Get the unmasked value of a specified field from a row identified by its ID.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @param fieldName the name of the masked field
     * @return the unmasked field value
     */
    @Override
    public String getUnmaskedField(String modelName, K id, String fieldName) {
        return (String) this.getUnmaskedFields(modelName, id, Collections.singletonList(fieldName)).get(fieldName);
    }

    /**
     * Get multiple unmasked field values from a row identified by its ID.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @param fields a list of masked fields to retrieve
     * @return a map of fieldName to unmasked value
     */
    @Override
    public Map<String, Object> getUnmaskedFields(String modelName, K id, List<String> fields) {
        fields.forEach(field -> {
            MetaField metaField = ModelManager.getModelField(modelName, field);
            Assert.notNull(metaField.getMaskingType(),
                    "The `maskingType` of model {0} field {1} is null, cannot invoke unmask!",
                    modelName, field);
        });
        Optional<Map<String, Object>> optionalValue = this.getById(modelName, id, fields);
        if (optionalValue.isEmpty()) {
            throw new IllegalArgumentException("The data of model {0} with id {1} does not exist!", modelName, id);
        }
        return optionalValue.get();
    }

    /**
     * Updates an existing row by its ID.
     *
     * @param modelName the name of the model
     * @param row the data containing updates
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean updateOne(String modelName, Map<String, Object> row) {
        return this.updateList(modelName, Collections.singletonList(row));
    }

    /**
     * Updates an existing row by its ID, then fetches the updated row from the database.
     *
     * @param modelName the name of the model
     * @param row the data containing updates
     * @param convertType the conversion type applied to the result
     * @return a map representing the updated row with the latest field values
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Map<String, Object> updateOneAndFetch(String modelName, Map<String, Object> row, ConvertType convertType) {
        return this.updateListAndFetch(modelName, Collections.singletonList(row), convertType).getFirst();
    }

    /**
     * Updates multiple rows by their IDs.
     * Each row in the list can specify different fields for update.
     *
     * @param modelName the name of the model
     * @param rows a list of data rows to update
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean updateList(String modelName, List<Map<String, Object>> rows) {
        Set<String> updatableFields = ModelManager.getModelUpdatableFields(modelName);
        Set<String> toUpdateFields = new HashSet<>();
        List<Map<String, Object>> toUpdateRows = new ArrayList<>();
        String pkField = ModelManager.getModelPrimaryKey(modelName);
        List<Serializable> pks = new ArrayList<>();
        rows.forEach(row -> {
            // Remove audit fields from the update data
            ModelConstant.AUDIT_FIELDS.forEach(row::remove);
            Set<String> rowFields = new HashSet<>(row.keySet());
            rowFields.retainAll(updatableFields);
            IdUtils.formatMapId(modelName, row);
            Serializable pk = (Serializable) row.get(pkField);
            Assert.notNull(pk, "When updating model {0}, the primary key {1} cannot be null! {2}", modelName, pkField, row);
            pks.add(pk);
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
        Integer updateCount;
        if (ModelManager.isTimelineModel(modelName)) {
            FlexQuery flexQuery = new FlexQuery(Arrays.asList(ModelConstant.ID, ModelConstant.SLICE_ID), new Filters().in(ModelConstant.SLICE_ID, pks));
            List<Map<String, Object>> sliceList = jdbcService.selectByFilter(modelName, flexQuery);
            Map<Serializable, Serializable> sliceMap = sliceList.stream()
                    .collect(Collectors.toMap(row -> (Serializable) row.get(ModelConstant.SLICE_ID), row -> (Serializable) row.get(ModelConstant.ID)));
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
     * Updates multiple rows by their IDs and then fetches them from the database.
     * Each row in the list can specify different fields for update.
     *
     * @param modelName the name of the model
     * @param rows a list of data rows to update
     * @param convertType the conversion type applied to the results
     * @return a list of updated rows with the latest field values
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<Map<String, Object>> updateListAndFetch(String modelName, List<Map<String, Object>> rows, ConvertType convertType) {
        this.updateList(modelName, rows);
        List<K> ids = Cast.of(rows.stream().map(r -> r.get(ModelConstant.ID)).collect(Collectors.toList()));
        return this.getRowsWithoutPermissionCheck(modelName, ids, Collections.emptyList(), convertType);
    }

    /**
     * Updates a single row identified by its business key.
     *
     * @param modelName the name of the model
     * @param row the data containing updates, including business key fields
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean updateByBusinessKey(String modelName, Map<String, Object> row) {
        Assert.notEmpty(row, "The update data for model {0} cannot be empty!", modelName);
        K id = this.getIdByBusinessKey(modelName, row);
        row.put(ModelConstant.ID, id);
        return this.updateOne(modelName, row);
    }

    /**
     * Updates multiple rows identified by external IDs.
     * Each row in the list can specify different fields for update.
     *
     * @param modelName the name of the model
     * @param rows a list of data rows to update, including external IDs
     * @return {@code true} if successful; otherwise an exception is thrown
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
     * Performs a batch update of rows that match the provided filters,
     * updating the fields specified in the value map.
     * <p>If no filters are specified, all data visible to the current user might be updated.</p>
     *
     * @param modelName the name of the model
     * @param filters optional filter criteria
     * @param value a map of field-value pairs to update
     * @return the number of rows affected
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
     * Deletes a single row by its ID.
     * If the model is a timeline model, all slices related to this ID are also deleted.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean deleteById(String modelName, K id) {
        return this.deleteByIds(modelName, Collections.singletonList(id));
    }

    /**
     * Deletes a single slice of a timeline model, identified by sliceId (the model's primary key).
     *
     * @param modelName the name of the model
     * @param sliceId the slice's unique ID
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean deleteBySliceId(String modelName, Serializable sliceId) {
        Assert.isTrue(ModelManager.isTimelineModel(modelName),
                "Model {0} is not a timeline model, and cannot delete slice.", modelName);
        TimelineSlice timelineSlice = timelineService.getTimelineSlice(modelName, sliceId);
        permissionService.checkIdsAccess(modelName, Collections.singletonList(timelineSlice.getId()), AccessType.DELETE);
        return timelineService.deleteSlice(modelName, timelineSlice);
    }

    /**
     * Deletes multiple rows by their IDs.
     *
     * @param modelName the name of the model
     * @param ids a list of unique IDs for the rows to delete
     * @return {@code true} if successful; otherwise an exception is thrown
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
     * Deletes a single row identified by its business key.
     *
     * @param modelName the name of the model
     * @param row the data containing the business key fields
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean deleteByBusinessKey(String modelName, Map<String, Object> row) {
        Assert.notEmpty(row, "The deletion data for model {0} cannot be empty!", modelName);
        K id = this.getIdByBusinessKey(modelName, row);
        return this.deleteById(modelName, id);
    }

    /**
     * Deletes a single row identified by its external ID.
     *
     * @param modelName the name of the model
     * @param externalId the external ID
     * @return {@code true} if successful; otherwise an exception is thrown
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean deleteByExternalId(String modelName, Serializable externalId) {
        return this.deleteByExternalIds(modelName, Collections.singletonList(externalId));
    }

    /**
     * Deletes multiple rows identified by their external IDs.
     *
     * @param modelName the name of the model
     * @param externalIds a list of external IDs
     * @return {@code true} if successful; otherwise an exception is thrown
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
     * Deletes rows matching the specified filters.
     *
     * @param modelName the name of the model
     * @param filters filtering conditions
     * @return {@code true} if successful; otherwise an exception is thrown
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
     * Copies a single row identified by its ID, returning the ID of the newly created copy.
     * <p>Read and create permissions are evaluated separately.</p>
     *
     * @param modelName the name of the model
     * @param id the source row's ID
     * @return the ID of the newly created copy
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public K copyById(String modelName, K id) {
        return this.copyByIds(modelName, Collections.singletonList(id)).getFirst();
    }

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
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Map<String, Object> copyByIdAndFetch(String modelName, K id, ConvertType convertType) {
        return this.copyByIdsAndFetch(modelName, Collections.singletonList(id), convertType).getFirst();
    }

    /**
     * Copies multiple rows based on their IDs and returns the IDs of the new copies.
     * <p>Read and create permissions are evaluated separately.</p>
     *
     * @param modelName the name of the model
     * @param ids a list of source row IDs
     * @return a list of IDs corresponding to the newly created copies
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
     * Copies multiple rows based on their IDs and returns the newly created data,
     * with the specified convert type applied.
     * <p>Read and create permissions are evaluated separately.</p>
     *
     * @param modelName the name of the model
     * @param ids a list of source row IDs
     * @param convertType the conversion type applied to the result
     * @return a list of maps representing the newly created rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<Map<String, Object>> copyByIdsAndFetch(String modelName, List<K> ids, ConvertType convertType) {
        List<K> newIds = this.copyByIds(modelName, ids);
        return this.getRowsWithoutPermissionCheck(modelName, newIds, Collections.emptyList(), convertType);
    }

    /**
     * Queries exactly one row matching the specified FlexQuery.
     * <p>If multiple rows match, an exception is thrown. This method is intended for internal code use.</p>
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} object defining fields, filters, sorting, etc.
     * @return a map representing the single matched row
     */
    @Override
    public Map<String, Object> searchOne(String modelName, FlexQuery flexQuery) {
        List<Map<String, Object>> rows = this.searchList(modelName, flexQuery);
        if (rows.isEmpty()) {
            return Collections.emptyMap();
        } else if (rows.size() > 1) {
            throw new IllegalArgumentException("The number of `searchOne` results for model {0} is greater than 1: {1}",
                    modelName, flexQuery.getFilters());
        }
        return rows.getFirst();
    }

    /**
     * Performs a non-paginated query based on FlexQuery, intended for internal code use.
     * <p>If the result set exceeds {@code MAX_BATCH_SIZE}, an error is logged but no exception is thrown.</p>
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} object defining fields, filters, sorting, etc.
     * @return a list of maps representing the matching rows
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
        List<Map<String, Object>> rows = jdbcService.selectByFilter(modelName, flexQuery);
        if (rows.size() > BaseConstant.MAX_BATCH_SIZE) {
            log.error("Model {} `searchList` exceeds the limit of {}, please switch to `searchPage`: {}",
                    modelName, BaseConstant.MAX_BATCH_SIZE, flexQuery);
        }
        return rows;
    }

    /**
     * Performs a non-paginated query based on FlexQuery, intended for internal code use.
     * <p>If the result set exceeds {@code MAX_BATCH_SIZE}, an error is logged but no exception is thrown.</p>
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} object defining fields, filters, sorting, etc.
     * @return a list of maps representing the matching rows
     */
    @Override
    public List<Map<String, Object>> searchName(String modelName, FlexQuery flexQuery) {
        List<String> displayFields = ModelManager.getModelDisplayName(modelName);
        flexQuery.select(displayFields);
        List<Map<String, Object>> rows = searchList(modelName, flexQuery);
        for (Map<String, Object> row : rows) {
            // Filter out field values for null or empty strings
            List<Object> displayValues = displayFields.stream().map(row::get)
                    .filter(v -> v != null && v != "").collect(Collectors.toList());
            String name = StringUtils.join(displayValues, StringConstant.DISPLAY_NAME_SEPARATOR);
            row.put(ModelConstant.DISPLAY_NAME, name);
        }
        return rows;
    }

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
    @Override
    public <R> List<R> searchList(String modelName, FlexQuery flexQuery, Class<R> returnClass) {
        List<Map<String, Object>> rows = this.searchList(modelName, flexQuery);
        return CollectionUtils.isEmpty(rows) ? Collections.emptyList() : BeanTool.mapListToObjects(rows, returnClass);
    }

    /**
     * Performs a paginated query based on FlexQuery, returning a {@link Page} of results.
     * <p>The page size must not exceed {@code MAX_BATCH_SIZE}.</p>
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} object defining fields, filters, sorting, etc.
     * @param page the {@link Page} containing pagination parameters
     * @return a {@link Page} of maps representing the queried rows
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
     * Queries data in a tree structure based on the specified FlexQuery.
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} object defining fields, filters, sorting, etc.
     * @return a list of maps representing the data in a hierarchical tree structure
     */
    @Override
    public List<Map<String, Object>> searchTree(String modelName, FlexQuery flexQuery) {
        return Collections.emptyList();
    }

    /**
     * Executes a PivotTable query based on the specified FlexQuery, returning up to
     * {@code DEFAULT_BATCH_SIZE} rows without pagination.
     *
     * @param modelName the name of the model
     * @param flexQuery a {@link FlexQuery} object defining fields, filters, sorting, grouping, splitting, summary, etc.
     * @return a {@link PivotTable} object encapsulating the pivot results
     */
    @Override
    public PivotTable searchPivot(String modelName, FlexQuery flexQuery) {
        List<Map<String, Object>> rows = this.searchList(modelName, flexQuery);
        // Aggregate operations into PivotTable structures
        return PivotTable.aggregateOperation(modelName, rows, flexQuery.getGroupBy(), flexQuery.getSplitBy());
    }

    /**
     * Counts the number of rows matching the given filters.
     *
     * @param modelName the name of the model
     * @param filters filtering conditions
     * @return the total count of matching rows
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
     * Checks if a row with the specified ID physically exists in the underlying data store,
     * without performing any permission checks.
     *
     * @param modelName the name of the model
     * @param id the unique ID of the row
     * @return {@code true} if the row physically exists; otherwise false
     */
    @Override
    public boolean exist(String modelName, K id) {
        return jdbcService.exist(modelName, id);
    }

    /**
     * Filters the provided list of IDs, returning only those that physically exist
     * in the data store, without performing any permission checks.
     *
     * @param modelName the name of the model
     * @param ids the collection of IDs to check
     * @return a list of IDs that exist
     */
    @Override
    public List<K> filterExistIds(String modelName, Collection<K> ids) {
        FlexQuery flexQuery = new FlexQuery(new Filters().in(ModelConstant.ID, ids));
        return jdbcService.getIds(modelName, ModelConstant.ID, flexQuery);
    }

}
