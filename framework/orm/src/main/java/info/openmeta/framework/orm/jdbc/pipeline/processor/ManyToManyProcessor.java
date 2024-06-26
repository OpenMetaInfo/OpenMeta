package info.openmeta.framework.orm.jdbc.pipeline.processor;

import com.google.common.collect.Sets;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.SubQuery;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.utils.IdUtils;
import info.openmeta.framework.orm.utils.ReflectTool;
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
 * <p>
 *     Field metadata can be configured with parameters:
 *      `autoBindMany` automatically binds the MANY end:
 *          when true, it automatically binds the associated value, and does not page,
 *          when False, the client needs to specify the subQuery and the result is paged.
 *      `autoExpandMany` automatically expands the MANY end:
 *          when true, read all fields of the associated model automatically,
 *          when False, just read [id, displayName] of the associated model by default, if there is no subQuery.
 */
@Slf4j
public class ManyToManyProcessor extends BaseProcessor {

    private final FlexQuery flexQuery;
    private SubQuery subQuery;

    public ManyToManyProcessor(MetaField metaField, FlexQuery flexQuery) {
        super(metaField);
        this.flexQuery = flexQuery;
        if (flexQuery != null) {
            this.subQuery = flexQuery.extractSubQuery(metaField.getFieldName());
        }
    }

    /**
     * Batch processing of ManyToMany input data.
     *
     * @param rows Input data list
     * @param accessType Access type, such as READ, CREATE, UPDATE
     */
    @Override
    public void batchProcessInputRows(List<Map<String, Object>> rows, AccessType accessType) {
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
                List<Serializable> linkIds = IdUtils.typeCastIds(valueList, metaField.getRelatedModel(), metaField.getInverseLinkField());
                linkIds.forEach(i -> mappingRows.add(
                        new HashMap<>(Map.of(metaField.getRelatedField(), id, metaField.getInverseLinkField(), i))
                ));
            }
        });
        ReflectTool.createList(metaField.getRelatedModel(), mappingRows);
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
                    List<Serializable> linkIds = IdUtils.typeCastIds(valueList, metaField.getRelatedModel(), metaField.getInverseLinkField());
                    if (mToMIdsMapping.containsKey(id)) {
                        // Remove the existing ids of the associated model to obtain the newLinkIds to be associated.
                        List<Serializable> newLinkIds = new ArrayList<>(linkIds);
                        newLinkIds.removeAll(mToMIdsMapping.get(id).keySet());
                        // The difference set means the relationship to be deleted.
                        List<Serializable> unlinkIds = new ArrayList<>(mToMIdsMapping.get(id).keySet());
                        unlinkIds.removeAll(linkIds);
                        if (!unlinkIds.isEmpty()) {
                            unlinkIds.forEach(i -> deleteMiddleIds.add(mToMIdsMapping.get(id).get(i)));
                        }
                        linkIds = newLinkIds;
                    }
                    linkIds.forEach(i -> newMToMRows.add(
                            new HashMap<>(Map.of(metaField.getRelatedField(), id, metaField.getInverseLinkField(), i))
                    ));
                }
            }
        });
        // Create middle table rows
        ReflectTool.createList(metaField.getRelatedModel(), newMToMRows);
        // Delete middle table rows
        ReflectTool.deleteList(metaField.getRelatedModel(), deleteMiddleIds);
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
        FlexQuery previousFlexQuery = new FlexQuery(fields, Filters.of(metaField.getRelatedField(), Operator.IN, ids));
        List<Map<String, Object>> previousMToMRows = ReflectTool.searchMapList(metaField.getRelatedModel(), previousFlexQuery);
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
        List<Serializable> mainModelIds = rows.stream().map(row -> (Serializable) row.get(ModelConstant.ID)).collect(Collectors.toList());
        List<Map<String, Object>> middleRows = getMiddleRows(mainModelIds);
        if (CollectionUtils.isEmpty(middleRows)) {
            rows.forEach(row -> row.put(fieldName, Collections.emptyList()));
            return;
        }
        // Expand the middle model rows, converted the `inverseLinkField` value to [id, displayName]
        // or {target row} according to the `autoExpandMany` config and `subQuery` object.
        List<Map<String, Object>> expandedMiddleRows = expandMiddleRowsWithAssociatedData(middleRows);
        // Group by `relatedField` of the middle model, which stores the main model id.
        Map<Serializable, List<Object>> groupedValues = groupMiddleRows(expandedMiddleRows);
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
     * @param mainModelIds Current table ids
     * @return Middle model rows: [[id, relatedField, inverseLinkField], ...]
     */
    private List<Map<String, Object>> getMiddleRows(List<Serializable> mainModelIds) {
        String relatedField = metaField.getRelatedField();
        String inverseLinkField = metaField.getInverseLinkField();
        Filters middleFilters = Filters.of(relatedField, Operator.IN, mainModelIds);
        Set<String> middleFields = Sets.newHashSet(relatedField, inverseLinkField);
        FlexQuery middleFlexQuery = new FlexQuery(middleFields, middleFilters);
        return ReflectTool.searchMapList(metaField.getRelatedModel(), middleFlexQuery);
    }

    /**
     * Query the middle model and associated model according to the mainModelIds.
     * By default, the `inverseLinkField` value is converted to [id, displayName].
     * When `autoExpandMany = true` or there is a `subQuery` based on the ManyToMany field,
     * the `inverseLinkField` value is converted to {inverseLinkField: {associated row}}
     *
     * @param middleRows Middle model rows
     * @return Middle model rows expanded with the associated model data: [{inverseLinkField: [id, displayName]}]
     *      or [{inverseLinkField: {associated row}}]
     */
    private List<Map<String, Object>> expandMiddleRowsWithAssociatedData(List<Map<String, Object>> middleRows) {
        String inverseLinkField = metaField.getInverseLinkField();
        List<Serializable> associatedIds = middleRows.stream().map(value -> (Serializable) value.get(inverseLinkField)).distinct().collect(Collectors.toList());
        String associatedModel = ModelManager.getModelField(metaField.getRelatedModel(), inverseLinkField).getRelatedModel();
        if (subQuery == null && !metaField.isAutoExpandMany()) {
            // When `subQuery == null` and `autoExpandMany = false`, get the displayNames of associated model directly.
            Map<Serializable, String> displayNames = ReflectTool.getDisplayNames(associatedModel, associatedIds, metaField.getDisplayName());
            // Update the `inverseLinkField` field of the middle model to {inverseLinkField: [id, displayName]}
            middleRows.forEach(r -> r.put(inverseLinkField, Arrays.asList((Serializable) r.get(inverseLinkField), displayNames.get((Serializable) r.get(inverseLinkField)))));
        } else {
            // Execute subQuery on the associated model.
            List<Map<String, Object>> associatedRows = this.getAssociatedRows(associatedModel, associatedIds);
            // Group the associated model rows by id
            Map<Serializable, Map<String, Object>> associatedRowMap = associatedRows.stream().collect(Collectors.toMap(row -> (Serializable) row.get(ModelConstant.ID), row -> row));
            // Update the `inverseLinkField` value of the middle model row, to {inverseLinkField: {associated row}}
            middleRows.forEach(r -> r.put(inverseLinkField, associatedRowMap.get((Serializable) r.get(inverseLinkField))));
        }
        return middleRows;
    }

    /**
     * Perform a subQuery on the associated model
     *
     * @param associatedModel Associated model
     * @param ids Associated model ids
     * @return Associated model rows
     */
    private List<Map<String, Object>> getAssociatedRows(String associatedModel, List<Serializable> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return Collections.emptyList();
        }
        Filters filters = Filters.in(ModelConstant.ID, ids);
        FlexQuery relatedFlexQuery;
        if (subQuery == null) {
            relatedFlexQuery = new FlexQuery(Collections.emptyList(), filters);
        } else {
            // When there is a subQuery condition, merge it together
            if (!CollectionUtils.isEmpty(subQuery.getFields()) && !subQuery.getFields().contains(ModelConstant.ID)) {
                subQuery.getFields().add(ModelConstant.ID);
            }
            filters.and(subQuery.getFilters());
            relatedFlexQuery = new FlexQuery(subQuery.getFields(), filters, subQuery.getOrders());
        }
        relatedFlexQuery.setConvertType(flexQuery.getConvertType());
        return ReflectTool.searchMapList(associatedModel, relatedFlexQuery);
    }

    /**
     * Group the expanded middle model rows by the `relatedField` attribute of the ManyToMany field,
     * which stores the main model id.
     *
     * @param expandedMiddleRows middle model rows, expanded with associated model data
     * @return Grouped middle model data: {mainModelId: [[id, displayName], ...]}
     *      or {mainModelId: [{associated row}, ...]
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
