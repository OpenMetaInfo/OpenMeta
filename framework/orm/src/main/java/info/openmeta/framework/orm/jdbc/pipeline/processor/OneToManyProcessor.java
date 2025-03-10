package info.openmeta.framework.orm.jdbc.pipeline.processor;

import com.google.common.collect.Sets;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.Cast;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.SubQuery;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.utils.BeanTool;
import info.openmeta.framework.orm.utils.IdUtils;
import info.openmeta.framework.orm.utils.ReflectTool;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

/**
 * OneToMany field processor.
 */
@Slf4j
public class OneToManyProcessor extends BaseProcessor {

    private final FlexQuery flexQuery;

    private SubQuery subQuery;

    @Getter
    private boolean changed = false;

    /**
     * Constructor of the OneToMany field processor object.
     *
     * @param metaField Field metadata object
     * @param flexQuery flexQuery object
     */
    public OneToManyProcessor(MetaField metaField, AccessType accessType, FlexQuery flexQuery) {
        super(metaField, accessType);
        this.flexQuery = flexQuery;
        if (flexQuery != null) {
            this.subQuery = flexQuery.extractSubQuery(metaField.getFieldName());
        }
    }

    /**
     * Batch processing of OneToMany input data
     *
     * @param rows Data list
     */
    @Override
    public void batchProcessInputRows(List<Map<String, Object>> rows) {
        if (AccessType.CREATE.equals(accessType)) {
            createWithRelatedRows(rows);
        } else if (AccessType.UPDATE.equals(accessType)) {
            updateWithRelatedRows(rows);
        }
    }

    /**
     * Batch create related model rows.
     *
     * @param rows Data list
     */
    private void createWithRelatedRows(List<Map<String, Object>> rows) {
        List<Map<String, Object>> createRows = new ArrayList<>();
        for (Map<String, Object> row : rows) {
            Serializable id = (Serializable) row.get(ModelConstant.ID);
            Object value = row.get(fieldName);
            if (value instanceof List<?> valueList && !valueList.isEmpty()) {
                List<Map<String, Object>> subRows;
                if (valueList.getFirst() instanceof Map<?, ?>) {
                    subRows = Cast.of(value);
                } else {
                    // Convert the list of objects to a list of maps
                    subRows = BeanTool.objectsToMapList(valueList);
                }
                subRows.forEach(subRow -> subRow.put(metaField.getRelatedField(), id));
                createRows.addAll(subRows);
            }
        }
        if (!CollectionUtils.isEmpty(createRows)) {
            changed = true;
            ReflectTool.createList(metaField.getRelatedModel(), createRows);
        }
    }

    /**
     * Batch update related model rows.
     * Query the existing related model rows, extract the createRows, updateRows, and deleteIds.
     *
     * @param rows Data list
     */
    private void updateWithRelatedRows(List<Map<String, Object>> rows) {
        List<Map<String, Object>> createRows = new ArrayList<>();
        List<Map<String, Object>> updateRows = new ArrayList<>();
        List<Serializable> deleteIds = new ArrayList<>();
        // {mainModelId: Set<relatedModelIds>}: get the existing OneToMany ids mapping.
        Map<Serializable, Set<Serializable>> existOneToManyIdsMap = this.getExistOneToManyMap(rows);
        for (Map<String, Object> row : rows) {
            Serializable id = (Serializable) row.get(ModelConstant.ID);
            Object value = row.get(fieldName);
            if (value instanceof List<?> valueList) {
                if (valueList.isEmpty() && existOneToManyIdsMap.containsKey(id)) {
                    // Delete related model rows when set OneToMany field to empty list.
                    deleteIds.addAll(existOneToManyIdsMap.get(id));
                } else {
                    List<Map<String, Object>> subRows;
                    if (valueList.getFirst() instanceof Map<?, ?>) {
                        subRows = Cast.of(value);
                    } else {
                        // Convert the list of objects to a list of maps
                        subRows = BeanTool.objectsToMapList(valueList);
                    }
                    if (existOneToManyIdsMap.containsKey(id)) {
                        // Process related model rows, when update the OneToMany field from old value to a new list.
                        processOneToManyUpdate(id, subRows, existOneToManyIdsMap, createRows, updateRows, deleteIds);
                    } else {
                        // Create related model rows, when update the OneToMany field from empty to a new list.
                        subRows.forEach(subRow -> subRow.put(metaField.getRelatedField(), id));
                        createRows.addAll(subRows);
                    }
                }
            }
        }
        List<?> newIds = ReflectTool.createList(metaField.getRelatedModel(), createRows);
        boolean updated = ReflectTool.updateList(metaField.getRelatedModel(), updateRows);
        boolean deleted = ReflectTool.deleteList(metaField.getRelatedModel(), deleteIds);
        if (updated || deleted || !newIds.isEmpty()) {
            changed = true;
        }
    }

    /**
     * Query the existing OneToMany rows and groupBy the main model id.
     *
     * @param rows Data list
     * @return {mainModelId: Set<relatedModelId>}
     */
    private Map<Serializable, Set<Serializable>> getExistOneToManyMap(Collection<Map<String, Object>> rows) {
        List<Serializable> ids = rows.stream().map(r -> (Serializable) r.get(ModelConstant.ID)).collect(Collectors.toList());
        Set<String> fields = Sets.newHashSet(ModelConstant.ID, metaField.getRelatedField());
        FlexQuery previousFlexQuery = new FlexQuery(fields, Filters.of(metaField.getRelatedField(), Operator.IN, ids));
        List<Map<String, Object>> previousOToMRows = ReflectTool.searchList(metaField.getRelatedModel(), previousFlexQuery);
        return previousOToMRows.stream().collect(Collectors.groupingBy(
                row -> (Serializable) row.get(metaField.getRelatedField()),
                Collectors.mapping(row -> (Serializable) row.get(ModelConstant.ID), Collectors.toSet())
        ));
    }

    /**
     * Update createRows, updateRows, and deleteIds when updating the OneToMany field.
     *
     * @param id mainModelId
     * @param subRows related model rows
     * @param existOneToManyIdsMap existing OneToMany ids mapping
     * @param createRows created rows of related model
     * @param updateRows updated rows of related model
     * @param deleteIds deleted ids of related models
     */
    private void processOneToManyUpdate(Serializable id, List<Map<String, Object>> subRows,
                                        Map<Serializable, Set<Serializable>> existOneToManyIdsMap, List<Map<String, Object>> createRows,
                                        List<Map<String, Object>> updateRows, List<Serializable> deleteIds) {
        Set<Serializable> currentSubIds = new HashSet<>();
        Set<Serializable> previousSubIds = existOneToManyIdsMap.get(id);
        subRows.forEach(subRow -> {
            Serializable subId = (Serializable) subRow.get(ModelConstant.ID);
            subId = IdUtils.formatId(metaField.getRelatedModel(), subId);
            if (previousSubIds.contains(subId)) {
                currentSubIds.add(subId);
                updateRows.add(subRow);
            } else {
                subRow.put(metaField.getRelatedField(), id);
                createRows.add(subRow);
            }
        });
        Set<Serializable> deletedSubIds = new HashSet<>(previousSubIds);
        deletedSubIds.removeAll(currentSubIds);
        deleteIds.addAll(deletedSubIds);
    }

    /**
     * Batch processing of OneToMany field outputting values
     *
     * @param rows Data list
     */
    @Override
    public void batchProcessOutputRows(List<Map<String, Object>> rows) {
        List<Serializable> mainModelIds = rows.stream()
                .map(row -> (Serializable) row.get(ModelConstant.ID))
                .toList();
        if (subQuery != null && Boolean.TRUE.equals(subQuery.getCount())) {
            expandRowsWithRelatedCount(mainModelIds, rows);
            return;
        }
        List<Map<String, Object>> relatedModelRows;
        if (subQuery == null && ConvertType.EXPAND_TYPES.contains(flexQuery.getConvertType())) {
            // Set the OneToMany field value to displayNames when not expanded by default.
            relatedModelRows = getRelatedRowsWithDisplayName(mainModelIds);
        } else {
            relatedModelRows = getRelatedModelRows(mainModelIds);
        }
        // {mainModelId: List<relatedModelRow>}, grouped related model rows by main model id
        Map<Serializable, List<Map<String, Object>>> groupedValues = groupByMainModelId(relatedModelRows);
        rows.forEach(row -> {
            List<Map<String, Object>> subRows = groupedValues.get((Serializable) row.get(ModelConstant.ID));
            if (CollectionUtils.isEmpty(subRows)) {
                subRows = Collections.emptyList();
            }
            row.put(fieldName, subRows);
        });
    }

    /**
     * Set the OneToMany field value to the count of related records.
     *
     * @param mainModelIds Main model ids
     * @param rows         Main model data list
     */
    private void expandRowsWithRelatedCount(List<Serializable> mainModelIds, List<Map<String, Object>> rows) {
        Filters filters = new Filters().in(metaField.getRelatedField(), mainModelIds);
        // When there is a subQuery filters, merge them with `AND` logic
        filters.and(subQuery.getFilters());
        // count subQuery on the joint model
        FlexQuery relatedFlexQuery = new FlexQuery(List.of(metaField.getRelatedField()), filters);
        // Count is automatically added during the groupBy operation
        relatedFlexQuery.setGroupBy(metaField.getRelatedField());
        List<Map<String, Object>> countRows = ReflectTool.searchList(metaField.getRelatedModel(), relatedFlexQuery);
        Map<Serializable, Integer> relatedCountMap = countRows.stream()
                .collect(Collectors.toMap(
                        row -> (Serializable) row.get(metaField.getRelatedField()),
                        row -> (Integer) row.get(ModelConstant.COUNT)));
        rows.forEach(row -> {
            row.put(fieldName, relatedCountMap.get((Serializable) row.get(ModelConstant.ID)));
        });
    }

    /**
     * Get the displayNames of related rows by the main model ids
     *
     * @param mainModelIds main model ids
     * @return related model rows
     */
    private List<Map<String, Object>> getRelatedRowsWithDisplayName(List<Serializable> mainModelIds) {
        FlexQuery relatedFlexQuery = new FlexQuery(new Filters().in(metaField.getRelatedField(), mainModelIds));
        // Set the convert type to REFERENCE.
        relatedFlexQuery.setConvertType(ConvertType.REFERENCE);
        relatedFlexQuery.select(metaField.getRelatedField());
        relatedFlexQuery.setKeepIdField(metaField.getRelatedField());
        return ReflectTool.searchName(metaField.getRelatedModel(), relatedFlexQuery);
    }

    /**
     * Query the related model rows by the main model ids
     *
     * @param mainModelIds main model ids
     * @return related model rows
     */
    private List<Map<String, Object>> getRelatedModelRows(List<Serializable> mainModelIds) {
        Filters filters = Filters.of(metaField.getRelatedField(), Operator.IN, mainModelIds);
        FlexQuery relatedFlexQuery;
        if (subQuery == null) {
            relatedFlexQuery = new FlexQuery(Collections.emptyList(), filters);
        } else {
            // When there is a subQuery filters, merge them with `AND` logic
            filters.and(subQuery.getFilters());
            relatedFlexQuery = new FlexQuery(subQuery.getFields(), filters, subQuery.getOrders());
            if (!CollectionUtils.isEmpty(subQuery.getFields())) {
                relatedFlexQuery.getFields().add(metaField.getRelatedField());
            }
            if (subQuery.getTopN() != null && subQuery.getTopN() > 0) {
                Assert.notNull(subQuery.getOrders(), "TopN query must have orderBy fields!");
                relatedFlexQuery.setTopN(subQuery.getTopN());
                // the `relatedField` field as the partition field of the TopN query, without aggregation
                relatedFlexQuery.setGroupBy(Collections.singletonList(metaField.getRelatedField()));
                relatedFlexQuery.setAggregate(false);
            }
        }
        relatedFlexQuery.setConvertType(flexQuery.getConvertType());
        // When get the related model rows of OneToMany field, the `relatedField` field of the related model is only
        // needed to obtain the ID for GroupBy, which might be a ManyToOne field defined in the related model.
        relatedFlexQuery.setKeepIdField(metaField.getRelatedField());
        return ReflectTool.searchList(metaField.getRelatedModel(), relatedFlexQuery);
    }

    /**
     * Group the related model rows by main model id.
     * The `relatedField` attribute of the OneToMany field is a field of the related model,
     * which stores the ID of the main model.
     *
     * @param relatedRows Related model rows
     * @return {mainModelId: List<relatedModelRow>}, grouped related model rows
     */
    private Map<Serializable, List<Map<String, Object>>> groupByMainModelId(List<Map<String, Object>> relatedRows) {
        String relatedField = metaField.getRelatedField();
        return relatedRows.stream().collect(Collectors.groupingBy(row -> (Serializable) row.get(relatedField)));
    }
}
