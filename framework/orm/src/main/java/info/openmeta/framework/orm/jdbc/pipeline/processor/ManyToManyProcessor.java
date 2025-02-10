package info.openmeta.framework.orm.jdbc.pipeline.processor;

import com.google.common.collect.Sets;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.SubQuery;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.utils.IdUtils;
import info.openmeta.framework.orm.utils.ReflectTool;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

/**
 * ManyToMany field value processing.
 * Structure of ManyToMany field:
 *      Main model --(ManyToMany)--> Associated model
 *      equals to:
 *      Main model --(OneToMany)--> Middle model --(ManyToOne)--> Associated model
 * The middle model is the mapping table of the ManyToMany field, configured in the `relatedModel` of ManyToMany
 * field metadata. The middle model contains the following key fields:
 *      id: Middle model id
 *      field1: Main model id (relatedField config in ManyToMany field metadata)
 *      field2: Associated model id (inverseLinkField config in ManyToMany field metadata)
 * <p>
 * The input parameters of ManyToMany field is the ids of associated model, that is: [id1, id2, id3],
 * which implies new mapping and deleted mapping maintained in the middle table. The new mapping and deleted mapping
 * are calculated by querying the middle table once, and comparing with the input ids.
 */
@Slf4j
public class ManyToManyProcessor extends BaseProcessor {

    private final FlexQuery flexQuery;

    private SubQuery subQuery;

    @Getter
    private boolean changed = false;

    public ManyToManyProcessor(MetaField metaField, AccessType accessType, FlexQuery flexQuery) {
        super(metaField, accessType);
        this.flexQuery = flexQuery;
        if (flexQuery != null) {
            this.subQuery = flexQuery.extractSubQuery(metaField.getFieldName());
        }
    }

    /**
     * Batch processing of ManyToMany input data.
     *
     * @param rows Input data list
     */
    @Override
    public void batchProcessInputRows(List<Map<String, Object>> rows) {
        if (AccessType.CREATE.equals(accessType)) {
            batchCreateMappingRows(rows);
        } else if (AccessType.UPDATE.equals(accessType)) {
            batchUpdateMappingRows(rows);
        }
    }

    /**
     * Batch CREATE mapping relationships, which is to create new rows in the middle table directly.
     *
     * @param rows Data list
     */
    private void batchCreateMappingRows(List<Map<String, Object>> rows) {
        List<Map<String, Object>> mappingRows = new ArrayList<>();
        rows.forEach(row -> {
            Serializable id = (Serializable) row.get(ModelConstant.ID);
            Object value = row.get(fieldName);
            if (value instanceof List<?> valueList && !valueList.isEmpty()) {
                List<Serializable> rightIds = IdUtils.formatIds(metaField.getMiddleModel(), metaField.getInverseLinkField(), valueList);
                rightIds.forEach(i -> mappingRows.add(
                        new HashMap<>(Map.of(metaField.getRelatedField(), id, metaField.getInverseLinkField(), i))
                ));
            }
        });
        if (!CollectionUtils.isEmpty(mappingRows)) {
            changed = true;
            ReflectTool.createList(metaField.getMiddleModel(), mappingRows);
        }
    }

    /**
     * Batch UPDATE mapping relationships.
     * The new mapping and deleted mapping are calculated by querying the middle table once,
     * and comparing with the input ids of the ManyToMany field.
     *
     * @param rows Data list
     */
    private void batchUpdateMappingRows(List<Map<String, Object>> rows) {
        // Map<mainModelId, Map<associatedModelId, middleTableId>>: get the existing mapping relationships.
        Map<Serializable, Map<Serializable, Serializable>> mToMIdsMapping = getPreviousManyToManyRows(rows);
        // Extract the new mapping rows: newMToMRows, and the middle table ids to be deleted: deleteMiddleIds
        List<Map<String, Object>> newMToMRows = new ArrayList<>();
        List<Serializable> deleteMiddleIds = new ArrayList<>();
        rows.forEach(row -> {
            Serializable id = (Serializable) row.get(ModelConstant.ID);
            Object value = row.get(fieldName);
            if (value instanceof List<?> valueList) {
                if (valueList.isEmpty() && mToMIdsMapping.containsKey(id)) {
                    // When the ManyToMany field value is an empty list, it means to clear the mapping table data
                    deleteMiddleIds.addAll(mToMIdsMapping.get(id).values());
                } else {
                    List<Serializable> rightIds = IdUtils.formatIds(metaField.getMiddleModel(), metaField.getInverseLinkField(), valueList);
                    if (mToMIdsMapping.containsKey(id)) {
                        // Remove the existing ids of the associated model to obtain the newLinkIds to be associated.
                        List<Serializable> newRightIds = new ArrayList<>(rightIds);
                        newRightIds.removeAll(mToMIdsMapping.get(id).keySet());
                        // The difference set means the relationship to be deleted.
                        List<Serializable> unlinkRightIds = new ArrayList<>(mToMIdsMapping.get(id).keySet());
                        unlinkRightIds.removeAll(rightIds);
                        if (!unlinkRightIds.isEmpty()) {
                            unlinkRightIds.forEach(i -> deleteMiddleIds.add(mToMIdsMapping.get(id).get(i)));
                        }
                        rightIds = newRightIds;
                    }
                    rightIds.forEach(i -> newMToMRows.add(
                            new HashMap<>(Map.of(metaField.getRelatedField(), id, metaField.getInverseLinkField(), i))
                    ));
                }
            }
        });
        if (!CollectionUtils.isEmpty(newMToMRows) || !CollectionUtils.isEmpty(deleteMiddleIds)) {
            changed = true;
            // Create middle table rows
            ReflectTool.createList(metaField.getMiddleModel(), newMToMRows);
            // Delete middle table rows
            ReflectTool.deleteList(metaField.getMiddleModel(), deleteMiddleIds);
        }
    }

    /**
     * Query original data from the middle table and grouped by the main model id.
     *
     * @param rows Input data list
     * @return mToMIdsMapping, `Map<mainModelId, Map<associatedModelId, middleTableId>>`,
     *      which is used to find the id of the middle table through relatedField (mainModelId)
     *      and inverseLinkField (associatedModelId) to delete the middle table rows.
     */
    private Map<Serializable, Map<Serializable, Serializable>> getPreviousManyToManyRows(Collection<Map<String, Object>> rows) {
        Map<Serializable, Map<Serializable, Serializable>> mToMIdsMapping = new HashMap<>();
        List<Serializable> ids = rows.stream().map(r -> (Serializable) r.get(ModelConstant.ID)).collect(Collectors.toList());
        Set<String> fields = Sets.newHashSet(ModelConstant.ID, metaField.getRelatedField(), metaField.getInverseLinkField());
        FlexQuery previousFlexQuery = new FlexQuery(fields).where(new Filters().in(metaField.getRelatedField(), ids));
        List<Map<String, Object>> previousMToMRows = ReflectTool.searchList(metaField.getMiddleModel(), previousFlexQuery);
        previousMToMRows.forEach(row -> {
            Serializable id = (Serializable) row.get(ModelConstant.ID);
            Serializable relatedId = (Serializable) row.get(metaField.getRelatedField());
            Serializable linkId = (Serializable) row.get(metaField.getInverseLinkField());
            if (mToMIdsMapping.containsKey(relatedId)) {
                mToMIdsMapping.get(relatedId).put(linkId, id);
            } else {
                mToMIdsMapping.put(relatedId, new HashMap<>(Map.of(linkId, id)));
            }
        });
        return mToMIdsMapping;
    }

    /**
     * Batch READ ManyToMany fields
     *
     * @param rows Data list
     */
    @Override
    public void batchProcessOutputRows(List<Map<String, Object>> rows) {
        List<Serializable> mainModelIds = rows.stream()
                .map(row -> (Serializable) row.get(ModelConstant.ID))
                .collect(Collectors.toList());
        List<Map<String, Object>> middleRows;
        Map<Serializable, List<Object>> groupedValues;
        if (subQuery != null && Boolean.TRUE.equals(subQuery.getCount())) {
            groupedValues = new HashMap<>();
            middleRows = getMiddleCount(mainModelIds);
            for (Map<String, Object> row : middleRows) {
                Serializable id = (Serializable) row.get(metaField.getRelatedField());
                row.remove(metaField.getRelatedField());
                List<Object> value = Collections.singletonList(row);
                groupedValues.put(id, value);
            }
        } else {
            middleRows = getMiddleRows(mainModelIds);
            if (CollectionUtils.isEmpty(middleRows)) {
                rows.forEach(row -> row.put(fieldName, Collections.emptyList()));
                return;
            }
            // Expand the middle model rows, according to the `subQuery` object.
            List<Map<String, Object>> expandedMiddleRows = expandMiddleRowsWithRightModelData(middleRows);
            // Group by `relatedField` of the middle model, which stores the main model id.
            groupedValues = groupMiddleRows(expandedMiddleRows);
        }
        rows.forEach(row -> {
            List<Object> associatedRows = groupedValues.get((Serializable) row.get(ModelConstant.ID));
            if (associatedRows == null) {
                associatedRows = Collections.emptyList();
            }
            // Update the ManyToMany field value with the associated model data
            row.put(fieldName, associatedRows);
        });
    }

    /**
     * Query the middle model according to the mainModelIds.
     * Since `relatedField` and `inverseLinkField` are both ManyToOne fields, this query directly get the id values
     * of the two fields in the database.
     *
     * @param mainModelIds Main model ids
     * @return Middle model rows: [[id, relatedField, inverseLinkField], ...]
     */
    private List<Map<String, Object>> getMiddleRows(List<Serializable> mainModelIds) {
        String relatedField = metaField.getRelatedField();
        String inverseLinkField = metaField.getInverseLinkField();
        Filters middleFilters = Filters.of(relatedField, Operator.IN, mainModelIds);
        Set<String> middleFields = Sets.newHashSet(relatedField, inverseLinkField);
        FlexQuery middleFlexQuery = new FlexQuery(middleFields, middleFilters);
        return ReflectTool.searchList(metaField.getMiddleModel(), middleFlexQuery);
    }

    /**
     * Query the middle model and right model according to the mainModelIds.
     * By default, the `inverseLinkField` value is converted to ModelReference object.
     *
     * @param middleRows Middle model rows
     * @return Middle model rows expanded with the right model data: [{inverseLinkField: ModelReference}]
     *      or [{inverseLinkField: {right row}}]
     */
    private List<Map<String, Object>> expandMiddleRowsWithRightModelData(List<Map<String, Object>> middleRows) {
        String inverseLinkField = metaField.getInverseLinkField();
        List<Serializable> rightIds = middleRows.stream()
                .map(value -> (Serializable) value.get(inverseLinkField))
                .distinct()
                .collect(Collectors.toList());
        if (CollectionUtils.isEmpty(rightIds)) {
            return Collections.emptyList();
        }
        // Execute subQuery on the right model.
        List<Map<String, Object>> rightRows = this.getRightRows(metaField.getRelatedModel(), rightIds);
        // Group the right model rows by id
        Map<Serializable, Map<String, Object>> rightRowMap = rightRows.stream()
                .collect(Collectors.toMap(row -> (Serializable) row.get(ModelConstant.ID), row -> row));
        // Update the `inverseLinkField` value of the middle model row, to {inverseLinkField: {associated row}}
        middleRows.forEach(r -> r.put(inverseLinkField, rightRowMap.get((Serializable) r.get(inverseLinkField))));
        return middleRows;
    }

    /**
     * Count the number of related rows in the middle model.
     *
     * @param mainModelIds Main model ids
     * @return Middle model count: [{relatedField, count}, ...]
     */
    private List<Map<String, Object>> getMiddleCount(List<Serializable> mainModelIds) {
        Filters filters = new Filters().in(ModelConstant.ID, mainModelIds);
        // When there is a subQuery filters, merge them with `AND` logic
        filters.and(subQuery.getFilters());
        // count subQuery on the middle model
        List<String> fields = new ArrayList<>(List.of(metaField.getRelatedField()));
        FlexQuery middleFlexQuery = new FlexQuery(fields, filters);
        // Count is automatically added during the groupBy operation
        middleFlexQuery.setGroupBy(metaField.getRelatedField());
        return ReflectTool.searchList(metaField.getMiddleModel(), middleFlexQuery);
    }

    /**
     * Perform a subQuery on the right model
     *
     * @param rightModel right model
     * @param ids right model ids
     * @return right model rows
     */
    private List<Map<String, Object>> getRightRows(String rightModel, List<Serializable> ids) {
        Filters filters = new Filters().in(ModelConstant.ID, ids);
        FlexQuery rightFlexQuery;
        if (subQuery == null) {
            rightFlexQuery = new FlexQuery(Collections.emptyList(), filters);
        } else {
            // When there is a subQuery filters, merge them with `AND` logic
            filters.and(subQuery.getFilters());
            rightFlexQuery = new FlexQuery(subQuery.getFields(), filters, subQuery.getOrders());
            if (!CollectionUtils.isEmpty(subQuery.getFields())) {
                rightFlexQuery.getFields().add(ModelConstant.ID);
            }
        }
        rightFlexQuery.setConvertType(flexQuery.getConvertType());
        return ReflectTool.searchList(rightModel, rightFlexQuery);
    }

    /**
     * Group the expanded middle model rows by the `relatedField` attribute of the ManyToMany field,
     * which stores the main model id.
     *
     * @param expandedMiddleRows middle model rows, expanded with right model data
     * @return Grouped middle model data: {mainModelId: [ModelReference, ...]}
     *      or {mainModelId: [{right row}, ...]
     */
    private Map<Serializable, List<Object>> groupMiddleRows(List<Map<String, Object>> expandedMiddleRows) {
        String relatedField = metaField.getRelatedField();
        String inverseLinkField = metaField.getInverseLinkField();
        return expandedMiddleRows.stream().collect(Collectors.groupingBy(
                row -> (Serializable) row.get(relatedField),
                Collectors.mapping(row -> row.get(inverseLinkField), Collectors.toList())
        ));
    }
}
