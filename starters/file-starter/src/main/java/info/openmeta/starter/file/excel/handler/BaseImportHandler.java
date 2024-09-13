package info.openmeta.starter.file.excel.handler;

import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.meta.MetaField;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Map;

/**
 * BaseImportHandler
 */
public abstract class BaseImportHandler {

    protected final String modelName;
    protected final String fieldName;
    protected final FieldType fieldType;
    protected final MetaField metaField;
    protected final boolean required;

    public BaseImportHandler(MetaField metaField, boolean required) {
        this.modelName = metaField.getModelName();
        this.fieldName = metaField.getFieldName();
        this.fieldType = metaField.getFieldType();
        this.metaField = metaField;
        this.required = required;
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
     *
     * @param row The row
     */
    public void handleRow(Map<String, Object> row) {
        Object value = row.get(fieldName);
        checkRequired(value);
        row.put(fieldName, handleValue(value));
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
     * Check required
     *
     * @param value The value
     */
    public void checkRequired(Object value) {
        if (required &&
                (value == null ||
                        (value instanceof String valueStr && !StringUtils.hasText(valueStr)))) {
            throw new IllegalArgumentException("The field `{0}` is required", fieldName);
        }
    }
}
