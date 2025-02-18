package info.openmeta.framework.orm.jdbc.pipeline.processor;

import com.google.common.collect.Sets;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.SubQuery;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.utils.IdUtils;
import info.openmeta.framework.orm.utils.ReflectTool;
import info.openmeta.framework.orm.vo.ModelReference;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

/**
 * ManyToMany field value processing.
 * Structure of ManyToMany field:
 *      Main model --(ManyToMany)--> Related model
 *      equals to:
 *      Main model <--(jointLeft field)-- Joint model --(jointRight field)--> Related model
 * The joint model is the mapping table of the ManyToMany field, configured in the `relatedModel` of ManyToMany
 * field metadata. The joint model contains the following key fields:
 *      id: Joint model id
 *      field1: Main model id (jointLeft config in ManyToMany field metadata)
 *      field2: Related model id (jointRight config in ManyToMany field metadata)
 * <p>
 * The input parameters of ManyToMany field is the ids of related model, that is: [id1, id2, id3],
 * which implies new mapping and deleted mapping maintained in the joint model. The new mapping and deleted mapping
 * are calculated by querying the joint model once, and comparing with the input ids.
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
     * Batch CREATE mapping relationships, which is to create new rows in the joint model directly.
     *
     * @param rows Data list
     */
    private void batchCreateMappingRows(List<Map<String, Object>> rows) {
        List<Map<String, Object>> mappingRows = new ArrayList<>();
        rows.forEach(row -> {
            Serializable id = (Serializable) row.get(ModelConstant.ID);
            Object value = row.get(fieldName);
            if (value instanceof List<?> valueList && !valueList.isEmpty()) {
                List<Serializable> rightIds = IdUtils.formatIds(metaField.getJointModel(), metaField.getJointRight(), valueList);
                rightIds.forEach(i -> mappingRows.add(
                        new HashMap<>(Map.of(metaField.getJointLeft(), id, metaField.getJointRight(), i))
                ));
            }
        });
        if (!CollectionUtils.isEmpty(mappingRows)) {
            changed = true;
            ReflectTool.createList(metaField.getJointModel(), mappingRows);
        }
    }

    /**
     * Batch UPDATE mapping relationships.
     * The new mapping and deleted mapping are calculated by querying the joint model once,
     * and comparing with the input ids of the ManyToMany field.
     *
     * @param rows Data list
     */
    private void batchUpdateMappingRows(List<Map<String, Object>> rows) {
        // Map<mainModelId, Map<relatedModelId, jointModelId>>: get the existing mapping relationships.
        Map<Serializable, Map<Serializable, Serializable>> mToMIdsMapping = getPreviousManyToManyRows(rows);
        // Extract the new mapping rows: newMToMRows, and the joint model ids to be deleted: deleteJointIds
        List<Map<String, Object>> newMToMRows = new ArrayList<>();
        List<Serializable> deleteJointIds = new ArrayList<>();
        rows.forEach(row -> {
            Serializable id = (Serializable) row.get(ModelConstant.ID);
            Object value = row.get(fieldName);
            if (value instanceof List<?> valueList) {
                if (valueList.isEmpty() && mToMIdsMapping.containsKey(id)) {
                    // When the ManyToMany field value is an empty list, it means to clear the mapping table data
                    deleteJointIds.addAll(mToMIdsMapping.get(id).values());
                } else {
                    List<Serializable> rightIds = IdUtils.formatIds(metaField.getJointModel(), metaField.getJointRight(), valueList);
                    if (mToMIdsMapping.containsKey(id)) {
                        // Remove the existing ids of the relatedModel to obtain the newRightIds to be jointed.
                        List<Serializable> newRightIds = new ArrayList<>(rightIds);
                        newRightIds.removeAll(mToMIdsMapping.get(id).keySet());
                        // The difference set means the relationship to be deleted.
                        List<Serializable> unlinkRightIds = new ArrayList<>(mToMIdsMapping.get(id).keySet());
                        unlinkRightIds.removeAll(rightIds);
                        if (!unlinkRightIds.isEmpty()) {
                            unlinkRightIds.forEach(i -> deleteJointIds.add(mToMIdsMapping.get(id).get(i)));
                        }
                        rightIds = newRightIds;
                    }
                    rightIds.forEach(i -> newMToMRows.add(
                            new HashMap<>(Map.of(metaField.getJointLeft(), id, metaField.getJointRight(), i))
                    ));
                }
            }
        });
        if (!CollectionUtils.isEmpty(newMToMRows) || !CollectionUtils.isEmpty(deleteJointIds)) {
            changed = true;
            // Create joint model rows
            ReflectTool.createList(metaField.getJointModel(), newMToMRows);
            // Delete joint model rows
            ReflectTool.deleteList(metaField.getJointModel(), deleteJointIds);
        }
    }

    /**
     * Query original data from the joint model and grouped by the main model id.
     *
     * @param rows Input data list
     * @return mToMIdsMapping, `Map<mainModelId, Map<relatedModelId, jointModelId>>`,
     *      which is used to find the id of the joint model through jointLeft (mainModelId)
     *      and jointRight (relatedModelId) to delete the joint model rows.
     */
    private Map<Serializable, Map<Serializable, Serializable>> getPreviousManyToManyRows(Collection<Map<String, Object>> rows) {
        Map<Serializable, Map<Serializable, Serializable>> mToMIdsMapping = new HashMap<>();
        List<Serializable> ids = rows.stream().map(r -> (Serializable) r.get(ModelConstant.ID)).collect(Collectors.toList());
        Set<String> fields = Sets.newHashSet(ModelConstant.ID, metaField.getJointLeft(), metaField.getJointRight());
        FlexQuery previousFlexQuery = new FlexQuery(fields).where(new Filters().in(metaField.getJointLeft(), ids));
        List<Map<String, Object>> previousMToMRows = ReflectTool.searchList(metaField.getJointModel(), previousFlexQuery);
        previousMToMRows.forEach(row -> {
            Serializable id = (Serializable) row.get(ModelConstant.ID);
            Serializable leftId = (Serializable) row.get(metaField.getJointLeft());
            Serializable rightId = (Serializable) row.get(metaField.getJointRight());
            if (mToMIdsMapping.containsKey(leftId)) {
                mToMIdsMapping.get(leftId).put(rightId, id);
            } else {
                mToMIdsMapping.put(leftId, new HashMap<>(Map.of(rightId, id)));
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
        List<Map<String, Object>> jointRows;
        Map<Serializable, List<Object>> groupedValues;
        if (subQuery == null && ConvertType.REFERENCE.equals(flexQuery.getConvertType())) {
            // Set the ManyToMany field value to Reference objects when the ManyToMany field is not expanded
            jointRows = getJointRows(mainModelIds);
            if (CollectionUtils.isEmpty(jointRows)) {
                rows.forEach(row -> row.put(fieldName, Collections.emptyList()));
                return;
            }
            List<Serializable> rightIds = jointRows.stream()
                    .map(value -> (Serializable) value.get(metaField.getJointRight()))
                    .distinct()
                    .toList();
            Map<Serializable, String> displayNames = ReflectTool.getDisplayNames(metaField.getRelatedModel(), rightIds);
            jointRows.forEach(row -> {
                Serializable rightId = (Serializable) row.get(metaField.getJointRight());
                row.put(metaField.getJointRight(), ModelReference.of(rightId, displayNames.get(rightId)));
            });
            groupedValues = groupJointRows(jointRows);
        } else if (subQuery != null && Boolean.TRUE.equals(subQuery.getCount())) {
            groupedValues = new HashMap<>();
            jointRows = getJointModelCount(mainModelIds);
            for (Map<String, Object> row : jointRows) {
                Serializable id = (Serializable) row.get(metaField.getJointLeft());
                row.remove(metaField.getJointLeft());
                List<Object> value = Collections.singletonList(row);
                groupedValues.put(id, value);
            }
        } else {
            jointRows = getJointRows(mainModelIds);
            if (CollectionUtils.isEmpty(jointRows)) {
                rows.forEach(row -> row.put(fieldName, Collections.emptyList()));
                return;
            }
            // Expand the joint model rows, according to the `subQuery` object.
            List<Map<String, Object>> expandedJointRows = expandJointRowsWithRightModelData(jointRows);
            // Group by `jointLeft` of the joint model, which stores the main model id.
            groupedValues = groupJointRows(expandedJointRows);
        }
        rows.forEach(row -> {
            List<Object> relatedRows = groupedValues.get((Serializable) row.get(ModelConstant.ID));
            if (relatedRows == null) {
                relatedRows = Collections.emptyList();
            }
            // Update the ManyToMany field value with the related model data
            row.put(fieldName, relatedRows);
        });
    }

    /**
     * Query the joint model according to the mainModelIds.
     * Since `jointLeft` and `jointRight` are both foreign key fields, this query directly get the id values
     * of the two fields in the database.
     *
     * @param mainModelIds Main model ids
     * @return Joint model rows: [[id, jointLeft, jointRight], ...]
     */
    private List<Map<String, Object>> getJointRows(List<Serializable> mainModelIds) {
        String jointLeft = metaField.getJointLeft();
        String jointRight = metaField.getJointRight();
        Filters jointFilters = Filters.of(jointLeft, Operator.IN, mainModelIds);
        Set<String> jointModelFields = Sets.newHashSet(jointLeft, jointRight);
        FlexQuery jointModelFlexQuery = new FlexQuery(jointModelFields, jointFilters);
        return ReflectTool.searchList(metaField.getJointModel(), jointModelFlexQuery);
    }

    /**
     * Query the joint model and right model according to the mainModelIds.
     * By default, the `jointRight` value is converted to ModelReference object.
     *
     * @param jointRows Joint model rows
     * @return Joint model rows expanded with the right model data: [{jointRight: ModelReference}]
     *      or [{jointRight: {right row}}]
     */
    private List<Map<String, Object>> expandJointRowsWithRightModelData(List<Map<String, Object>> jointRows) {
        String jointRight = metaField.getJointRight();
        List<Serializable> rightIds = jointRows.stream()
                .map(value -> (Serializable) value.get(jointRight))
                .distinct()
                .toList();
        if (CollectionUtils.isEmpty(rightIds)) {
            return Collections.emptyList();
        }
        // Execute subQuery on the right model.
        List<Map<String, Object>> rightRows = this.getRightRows(metaField.getRelatedModel(), rightIds);
        // Group the right model rows by id
        Map<Serializable, Map<String, Object>> rightRowMap = rightRows.stream()
                .collect(Collectors.toMap(row -> (Serializable) row.get(ModelConstant.ID), row -> row));
        // Update the `jointRight` value of the joint model row, to {jointRight: {right model row}}
        jointRows.forEach(r -> r.put(jointRight, rightRowMap.get((Serializable) r.get(jointRight))));
        return jointRows;
    }

    /**
     * Count the number of related rows in the joint model.
     *
     * @param mainModelIds Main model ids
     * @return Joint model count: [{jointLeft, count}, ...]
     */
    private List<Map<String, Object>> getJointModelCount(List<Serializable> mainModelIds) {
        Filters filters = new Filters().in(ModelConstant.ID, mainModelIds);
        // When there is a subQuery filters, merge them with `AND` logic
        filters.and(subQuery.getFilters());
        // count subQuery on the joint model
        List<String> fields = new ArrayList<>(List.of(metaField.getJointLeft()));
        FlexQuery jointModelFlexQuery = new FlexQuery(fields, filters);
        // Count is automatically added during the groupBy operation
        jointModelFlexQuery.setGroupBy(metaField.getJointLeft());
        return ReflectTool.searchList(metaField.getJointModel(), jointModelFlexQuery);
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
     * Group the expanded joint model rows by the `jointLeft` attribute of the ManyToMany field,
     * which stores the main model id.
     *
     * @param expandedJointRows joint model rows, expanded with right model data
     * @return Grouped joint model data: {mainModelId: [ModelReference, ...]}
     *      or {mainModelId: [{right row}, ...]
     */
    private Map<Serializable, List<Object>> groupJointRows(List<Map<String, Object>> expandedJointRows) {
        String jointLeft = metaField.getJointLeft();
        String jointRight = metaField.getJointRight();
        return expandedJointRows.stream().collect(Collectors.groupingBy(
                row -> (Serializable) row.get(jointLeft),
                Collectors.mapping(row -> row.get(jointRight), Collectors.toList())
        ));
    }
}
