package info.openmeta.framework.orm.jdbc.pipeline.processor;

import com.google.common.collect.Sets;
import info.openmeta.framework.base.constant.StringConstant;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.SubQuery;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.utils.IdUtils;
import info.openmeta.framework.orm.utils.ReflectTool;
import info.openmeta.framework.orm.vo.ModelReference;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;

/**
 * OneToOne/ManyToOne field group processor.
 * Combine the OneToOne/ManyToOne field itself, cascaded fields depend on it,
 * and the custom cascaded fields that reference it.
 */
@Slf4j
public class XToOneGroupProcessor extends BaseProcessor {

    /**
     * Cascaded fields depending on the same OneToOne/ManyToOne field, added by XToOneGroupProcessorFactory.
     * READ: calculate and return the dynamic cascaded fields.
     * CREATE/UPDATE: calculate the stored cascaded fields.
     */
    private final List<MetaField> cascadedFields = new ArrayList<>(0);
    // The fields of the related model that need to be expanded through the OneToOne/ManyToOne field.
    private final Set<String> expandFields = Sets.newHashSet(ModelConstant.ID);
    private final FlexQuery flexQuery;

    /**
     * Constructor of the ManyToOne/OneToOne group processor.
     *
     * @param metaField xToOne field metadata object
     * @param accessType Access type
     * @param flexQuery flexQuery object
     */
    public XToOneGroupProcessor(MetaField metaField, AccessType accessType, FlexQuery flexQuery) {
        super(metaField, accessType);
        this.flexQuery = flexQuery;
        // For ManyToOne/OneToOne fields, get the displayName of the related model.
        this.expandFields.addAll(ModelManager.getModelDisplayName(metaField.getRelatedModel()));
        if (flexQuery != null) {
            // When the flexQuery has subQuery base on the ManyToOne/OneToOne field,
            // expand the fields of the related model to be read.
            SubQuery subQuery = flexQuery.extractSubQuery(metaField.getFieldName());
            if (subQuery != null && !CollectionUtils.isEmpty(subQuery.getFields())) {
                this.expandFields.addAll(subQuery.getFields());
            }
        }
    }

    /**
     * Add the cascaded field of the related model to the expandFields.
     * For example, add the `name` field in `deptId.name` to the expandFields.
     *
     * @param cascadedField Cascaded field metadata object
     */
    public void addCascadedField(MetaField cascadedField) {
        this.cascadedFields.add(cascadedField);
        this.expandFields.add(cascadedField.getDependentFields().get(1));
    }

    /**
     * Batch process the input data of the cascaded fields and ManyToOne/OneToOne fields.
     *
     * @param rows Data collection
     */
    public void batchProcessInputRows(List<Map<String, Object>> rows) {
        if (!cascadedFields.isEmpty()) {
            Map<Serializable, Map<String, Object>> relatedRowMap = getRelatedModelRowMap(rows);
            // Calculate the stored cascaded field first.
            cascadedFields.forEach(field -> batchProcessOutputRowsCascaded(field, rows, relatedRowMap));
        }
        XToOneProcessor xToOneField = new XToOneProcessor(metaField, accessType);
        xToOneField.batchProcessInputRows(rows);
    }

    /**
     * Batch process the output data of the cascaded field and the ManyToOne/OneToOne field.
     *
     * @param rows The list of output data
     */
    public void batchProcessOutputRows(List<Map<String, Object>> rows) {
        if (flexQuery.isAcrossTimeline()
                && ModelManager.isTimelineModel(metaField.getModelName())
                && ModelManager.isTimelineModel(metaField.getRelatedModel())
                && ConvertType.EXPAND_TYPES.contains(flexQuery.getConvertType())) {
            // When the main model and related model are both timeline models in a cross-timeline query,
            // the fields of the related model `displayNames` has been queried, compute the displayName value directly.
            processRelatedTimeLineModelDisplayName(rows);
        } else {
            Map<Serializable, Map<String, Object>> relatedRowMap = getRelatedModelRowMap(rows);
            // Before expand the ManyToOne/OneToOne field, calculate the dynamic cascaded field first.
            cascadedFields.forEach(sf -> batchProcessOutputRowsCascaded(sf, rows, relatedRowMap));
            XToOneProcessor xToOneField = new XToOneProcessor(metaField, accessType, flexQuery);
            // Expand the ManyToOne/OneToOne field with displayName, or related model row according to subQuery.
            xToOneField.batchProcessOutputRows(rows, relatedRowMap);
        }
    }

    /**
     * Get the mapping of related model rows.
     *
     * @param rows Data list
     * @return {relatedModelId: {related model row}}
     */
    public Map<Serializable, Map<String, Object>> getRelatedModelRowMap(List<Map<String, Object>> rows) {
        Map<Serializable, Map<String, Object>> relatedValueMap = new HashMap<>();
        Set<Serializable> relatedIds = rows.stream().map(r -> (Serializable) r.get(metaField.getFieldName()))
                .filter(IdUtils::validId).collect(Collectors.toSet());
        if (!CollectionUtils.isEmpty(relatedIds)) {
            // Query related model rows using filter: ["id", "IN", relatedIds]
            Filters filters = Filters.of(ModelConstant.ID, Operator.IN, relatedIds);
            FlexQuery relatedFlexQuery = new FlexQuery(this.expandFields, filters);
            if (flexQuery != null) {
                relatedFlexQuery.setConvertType(flexQuery.getConvertType());
            }
            List<Map<String, Object>> relatedRows= ReflectTool.searchList(metaField.getRelatedModel(), relatedFlexQuery);
            relatedRows.forEach(row -> relatedValueMap.put((Serializable) row.get(ModelConstant.ID), row));
        }
        return relatedValueMap;
    }

    /**
     * Batch process the Value of the cascadedFields.
     * READ: the stored cascaded fields.
     * CREATE/UPDATE: the dynamic cascaded fields.
     *
     * @param cascadedField Cascaded field object
     * @param rows Data list
     * @param relatedValueMap related model data: {relatedModelId: {related model row}}
     */
    private void batchProcessOutputRowsCascaded(MetaField cascadedField, List<Map<String, Object>> rows, Map<Serializable, Map<String, Object>> relatedValueMap) {
        List<String> casFields = cascadedField.getDependentFields();
        String xToOneFieldName = casFields.getFirst();
        String cascadeModel = ModelManager.getModelField(modelName, xToOneFieldName).getRelatedModel();
        rows.forEach(row -> {
            Serializable id = (Serializable) row.get(xToOneFieldName);
            Object value;
            if (IdUtils.validId(id)) {
                id = IdUtils.formatId(cascadeModel, id);
                if (relatedValueMap.containsKey(id)) {
                    value = relatedValueMap.get(id).get(casFields.get(1));
                } else {
                    log.warn("Model {}, the {} field value does not exist in the related model {}: {}",
                            metaField.getModelName(), casFields.getFirst(), metaField.getRelatedModel(), row);
                    value = null;
                }
            } else {
                value = null;
            }
            row.put(cascadedField.getFieldName(), value);
        });
    }

    /**
     * When the main model and related model are both timeline models in a `acrossTimeline` query,
     * the `displayNames` fields of the related timeline model has been queried,
     * and the displayName is directly computed and assigned to the ManyToOne/OneToOne field.
     *
     * @param rows Data list
     */
    public void processRelatedTimeLineModelDisplayName(List<Map<String, Object>> rows) {
        List<String> displayFields = ModelManager.getModelDisplayName(metaField.getRelatedModel());
        String xToOneFieldName = metaField.getFieldName();
        rows.forEach(row -> {
            Map<String, Object> relatedRow = new HashMap<>();
            List<String> cascadedTimeLineFields = new ArrayList<>();
            row.forEach((key, value) -> {
                if (key.contains(".") && key.startsWith(xToOneFieldName)) {
                    // Only process the timeline model `displayName` fields with `.` separator flag, such as deptId.name
                    String[] fields = StringUtils.split(key, ".");
                    relatedRow.put(fields[1], value);
                    cascadedTimeLineFields.add(key);
                }
            });
            if (!relatedRow.isEmpty()) {
                // Compute the displayName of related timeline model, and assign it to the ManyToOne/OneToOne field.
                List<Object> displayValues = displayFields.stream().map(relatedRow::get).filter(n -> n != null && n != "").collect(Collectors.toList());
                String displayName = StringUtils.join(displayValues, StringConstant.DISPLAY_NAME_SEPARATOR);
                Serializable relatedId = (Serializable) row.get(xToOneFieldName);
                Object value = ConvertType.REFERENCE.equals(flexQuery.getConvertType()) ?
                        ModelReference.of(relatedId, displayName) : displayName;
                row.put(xToOneFieldName, value);
            }
            // After expanding the ManyToOne/OneToOne field, remove the `.` separator flag field specified in the `displayName` fields
            cascadedTimeLineFields.forEach(row::remove);
        });
    }

}
