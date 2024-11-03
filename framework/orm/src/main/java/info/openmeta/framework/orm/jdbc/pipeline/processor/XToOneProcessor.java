package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.constant.StringConstant;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.SubQuery;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.utils.IdUtils;
import info.openmeta.framework.orm.vo.ModelReference;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * ManyToOne/OneToOne field processor.
 * Get the displayName of ManyToOne/OneToOne field.
 */
@Slf4j
public class XToOneProcessor extends BaseProcessor {

    private ConvertType convertType = ConvertType.TYPE_CAST;
    private SubQuery subQuery;

    /**
     * Constructor of the input data field processor object.
     *
     * @param metaField Field metadata object
     * @param accessType Access type
     */
    public XToOneProcessor(MetaField metaField, AccessType accessType) {
        super(metaField, accessType);
    }

    /**
     * Constructor of the ManyToOne/OneToOne field processor object.
     *
     * @param metaField Field metadata object
     * @param accessType Access type
     * @param flexQuery flexQuery object
     */
    public XToOneProcessor(MetaField metaField, AccessType accessType, FlexQuery flexQuery) {
        super(metaField, accessType);
        this.convertType = flexQuery.getConvertType();
        this.subQuery = flexQuery.extractSubQuery(metaField.getFieldName());
    }

    /**
     * Process one inputting row, convert Integer id to Long.
     *
     * @param row        data row
     */
    @Override
    public void processInputRow(Map<String, Object> row) {
        checkReadonly(row);
        if (row.containsKey(fieldName) && row.get(fieldName) != null) {
            row.compute(fieldName, (k, id) -> IdUtils.convertIdToLong(id));
        } else if (AccessType.CREATE.equals(accessType)) {
            checkRequired(row);
            row.put(fieldName, metaField.getDefaultValueObject());
        } else if (row.containsKey(fieldName)){
            checkRequired(row);
            row.put(fieldName, getFieldTypeDefaultValue());
        }
    }

    /**
     * Expand the ManyToOne/OneToOne field with displayName, or related model row according to subQuery.
     *
     * @param rows Data list
     * @param relatedRowMap related model row map: {id: {field: value}}
     */
    public void batchProcessOutputRows(List<Map<String, Object>> rows, Map<Serializable, Map<String, Object>> relatedRowMap) {
        if (subQuery != null) {
            // When the subQuery is not null, assign the subQuery result to the ManyToOne/OneToOne field directly.
            rows.forEach(row -> row.put(fieldName, relatedRowMap.get((Serializable) row.get(ModelConstant.ID))));
        } else if (ConvertType.EXPAND_TYPES.contains(convertType)) {
            // If subQuery is null, but the result need to be expanded, fill in the ManyToOne/OneToOne with displayName.
            Map<Serializable, String> displayNameMap = this.getDisplayNameMap(relatedRowMap);
            rows.forEach(row -> processOutputRow(row, displayNameMap));
        }
    }

    /**
     * Fill in the displayName of ManyToOne/OneToOne field.
     *
     * @param row row data
     * @param displayNameMap the displayName map of the related model: {id: displayName}
     */
    public void processOutputRow(Map<String, Object> row, Map<Serializable, String> displayNameMap) {
        if (!row.containsKey(fieldName) || row.get(fieldName) == null) {
            return;
        }
        Serializable id = (Serializable) row.get(fieldName);
        id = IdUtils.convertIdToLong(id);
        Object value = null;
        if (displayNameMap.containsKey(id)) {
            value = displayNameMap.get(id);
            if (ConvertType.REFERENCE.equals(convertType)) {
                value = ModelReference.of(id, (String) value);
            }
        } else if (IdUtils.validId(id)) {
            log.warn("Model data {}(id={}) with field {}={}, the field value not exist in related model {}!",
                    metaField.getModelName(), row.get(ModelConstant.ID), fieldName, id, metaField.getRelatedModel());
        }
        row.put(fieldName, value);
    }

    /**
     * Get the displayName map of the associated model: {id: displayName}
     *
     * @param relatedValueMap Field values of the associated model {id: {fieldName: Value}}
     * @return displayNameMap {id: displayName}
     */
    private Map<Serializable, String> getDisplayNameMap(Map<Serializable, Map<String, Object>> relatedValueMap) {
        Map<Serializable, String> displayNameMap = new HashMap<>();
        List<String> displayFields = ModelManager.getFieldDisplayName(metaField);
        for (Map.Entry<Serializable, Map<String, Object>> value : relatedValueMap.entrySet()) {
            List<Object> displayValues = displayFields.stream().map(value.getValue()::get)
                    .filter(n -> n != null && n != "").collect(Collectors.toList());
            String name = StringUtils.join(displayValues, StringConstant.DISPLAY_NAME_SEPARATOR);
            displayNameMap.put(value.getKey(), name);
        }
        return displayNameMap;
    }
}
