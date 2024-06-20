package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.orm.meta.MetaField;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.Map;

/**
 * Base field processor class
 */
public abstract class BaseProcessor implements FieldProcessor {

    protected final String modelName;
    protected final String fieldName;
    protected final MetaField metaField;

    /**
     * Field processor object constructor
     *
     * @param metaField field metadata object
     */
    public BaseProcessor(MetaField metaField) {
        this.metaField = metaField;
        this.modelName = metaField.getModelName();
        this.fieldName = metaField.getFieldName();
    }

    /**
     * Get the default value of the field processor, used to replace null value.
     *
     * @return default value
     */
    protected Object getFieldTypeDefaultValue() {
        return metaField.getFieldType().getDefaultValue();
    }

    /**
     * The readonly field cannot be assigned a value.
     */
    protected void checkReadonly(Map<String, Object> row) {
        if (metaField.getReadonly()
                && row.containsKey(fieldName)
                && !metaField.getComputed()
                && StringUtils.isBlank(metaField.getCascadedField())) {
            throw new IllegalArgumentException("Model field {0}:{1} is a readonly field and cannot be assigned!",
                    metaField.getModelName(), fieldName);
        }
    }

    /**
     * The required field cannot be assigned a null value.
     */
    protected void checkRequired(Map<String, Object> row) {
        if (metaField.getRequired() && row.get(fieldName) == null) {
            throw new IllegalArgumentException("Model field {0}:{1} is a required field and cannot be null!",
                    metaField.getModelName(), fieldName);
        }
    }

    /**
     * Process a single-row input data.
     *
     * @param row The single-row data to be created or updated
     * @param accessType Access type, such as READ, CREATE, UPDATE
     */
    public void processInputRow(Map<String, Object> row, AccessType accessType) {
        checkReadonly(row);
        if (AccessType.CREATE.equals(accessType)) {
            checkRequired(row);
            row.computeIfAbsent(fieldName, k -> metaField.getDefaultValueObject());
        } else if (row.containsKey(fieldName) && row.get(fieldName) == null) {
            // Check if the required field is set to null.
            checkRequired(row);
            // Set the default value of the fieldType when the field is null.
            row.put(fieldName, getFieldTypeDefaultValue());
        }
    }

    /**
     * Batch process input data
     *
     * @param rows The list of data to be created or updated
     * @param accessType Access type, such as READ, CREATE, UPDATE
     */
    public void batchProcessInputRows(List<Map<String, Object>> rows, AccessType accessType) {
        rows.forEach(row -> processInputRow(row, accessType));
    }

    /**
     * Process a single-row output data.
     *
     * @param row The single-row output data
     */
    public void processOutputRow(Map<String, Object> row) {
        if (row.containsKey(fieldName) && row.get(fieldName) == null) {
            row.put(fieldName, getFieldTypeDefaultValue());
        }
    }

    /**
     * Batch process output data
     *
     * @param rows The list of output data
     */
    public void batchProcessOutputRows(List<Map<String, Object>> rows) {
        rows.forEach(this::processOutputRow);
    }
}
