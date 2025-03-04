package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.meta.MetaField;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * Processor for multiple string fields. Such as MultiString and MultiOption fields.
 */
public class MultiStringProcessor extends BaseProcessor {

    private final ConvertType convertType;

    public MultiStringProcessor(MetaField metaField, AccessType accessType, ConvertType convertType) {
        super(metaField, accessType);
        this.convertType = convertType;
    }

    /**
     * Check if the required field is set to null or empty.
     */
    @Override
    protected void checkRequired(Map<String, Object> row) {
        if (metaField.isRequired() && StringUtils.isBlank((String) row.get(fieldName))) {
            throw new IllegalArgumentException("Model required field {0}:{1} cannot be empty!", metaField.getModelName(), fieldName);
        }
    }

    /**
     * Convert the multiple value to a string for storage.
     *
     * @param row Single-row data to be updated
     */
    @Override
    public void processInputRow(Map<String, Object> row) {
        checkReadonly(row);
        Object value = row.get(fieldName);
        if (value instanceof List) {
            row.put(fieldName, StringUtils.join((List<?>) value, ","));
        } else if (value instanceof String) {
            row.put(fieldName, value);
        } else if (AccessType.CREATE.equals(accessType)) {
            checkRequired(row);
            row.computeIfAbsent(fieldName, k -> metaField.getDefaultValueObject());
        }
    }

    /**
     * Convert the string value of MultiString field to a list.
     *
     * @param row Single-row output data
     */
    @Override
    public void processOutputRow(Map<String, Object> row) {
        if (!row.containsKey(fieldName)) {
            return;
        } else if (ConvertType.DISPLAY.equals(convertType) &&
                (FieldType.MULTI_STRING.equals(metaField.getFieldType()) ||
                        FieldType.MULTI_FILE.equals(metaField.getFieldType()))) {
            return;
        }
        String value = (String) row.get(fieldName);
        Object result = StringUtils.isBlank(value) ? null : Arrays.asList(StringUtils.split(value, ","));
        row.put(fieldName, result);
    }

}
