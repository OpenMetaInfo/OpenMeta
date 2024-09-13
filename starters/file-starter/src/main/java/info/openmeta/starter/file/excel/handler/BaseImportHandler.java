package info.openmeta.starter.file.excel.handler;

import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.meta.MetaField;

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

    public BaseImportHandler(MetaField metaField) {
        this.modelName = metaField.getModelName();
        this.fieldName = metaField.getFieldName();
        this.fieldType = metaField.getFieldType();
        this.metaField = metaField;
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
        row.put(fieldName, handleValue(row.get(fieldName)));
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
}
