package info.openmeta.starter.file.excel.handler;

import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.starter.file.dto.ImportFieldDTO;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Map;

/**
 * BaseImportHandler
 */
public abstract class BaseImportHandler {

    protected final String modelName;
    protected final String fieldName;
    protected final MetaField metaField;
    protected final ImportFieldDTO importFieldDTO;

    public BaseImportHandler(MetaField metaField, ImportFieldDTO importFieldDTO) {
        this.modelName = metaField.getModelName();
        this.fieldName = metaField.getFieldName();
        this.metaField = metaField;
        this.importFieldDTO = importFieldDTO;
    }

    /**
     * Handle the rows
     *
     * @param rows The rows
     */
    public void handleRows(List<Map<String, Object>> rows) {
        rows.forEach(this::handleRow);
    }

    /**
     * Handle the row
     * Properties of the row will be modified:
     *      - Check if the field is required
     *      - Set the default value if the field is empty and the default value is set
     *      - Remove the field if the field is empty and ignoreEmpty is true
     *
     * @param row The row
     */
    public void handleRow(Map<String, Object> row) {
        Object value = row.get(fieldName);
        boolean isEmpty = valueIsEmpty(value);
        if (isEmpty) {
            checkRequired();
            if (StringUtils.hasText(importFieldDTO.getDefaultValue())) {
                Object defaultValue = FieldType.convertStringToObject(metaField.getFieldType(), importFieldDTO.getDefaultValue());
                row.put(fieldName, defaultValue);
            } else if (Boolean.TRUE.equals(importFieldDTO.getIgnoreEmpty())) {
                row.remove(fieldName);
            }
        } else {
            row.put(fieldName, handleValue(value));
        }
    }

    /**
     * Handle the value of the field
     *
     * @param value The value
     * @return The handled value
     */
    public Object handleValue(Object value) {
        // handle value
        return value;
    }

    /**
     * Check whether the value is empty
     *
     * @param value The value
     * @return Whether the value is empty
     */
    public boolean valueIsEmpty(Object value) {
        return value == null || (value instanceof String valueStr && !StringUtils.hasText(valueStr));
    }


    /**
     * Check required
     */
    public void checkRequired() {
        if (Boolean.TRUE.equals(importFieldDTO.getRequired())) {
            throw new IllegalArgumentException("The field `{0}` is required", fieldName);
        }
    }

}
