package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.constant.StringConstant;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.meta.MetaField;
import org.apache.commons.lang3.StringUtils;

import java.util.Map;

/**
 * String field processor
 */
public class StringProcessor extends BaseProcessor {

    public StringProcessor(MetaField metaField, AccessType accessType) {
        super(metaField, accessType);
    }

    /**
     * Check if the string length exceeds the limit, excluding default length, which might be 0.
     * @param value String data
     */
    private void checkStringLength(String value) {
        if (value.length() > metaField.getLength() && metaField.getLength() > 0) {
            throw new BusinessException("""
                    Model field {0}: {1} length exceeds the limit of {2}. The actual length is {3}, value: {4}""",
                    metaField.getModelName(), metaField.getFieldName(), metaField.getLength(), value.length(), value);
        }
    }

    /**
     * String field length check
     */
    @Override
    public void processInputRow(Map<String, Object> row) {
        checkReadonly(row);
        Object obj = row.get(fieldName);
        String value;
        if (obj instanceof Enum) {
            value = StringTools.toUpperCamelCase(((Enum<?>) obj).name());
        } else if (obj instanceof String) {
            value = (String) obj;
        } else {
            value = obj == null ? null : String.valueOf(obj);
        }
        if (StringUtils.isNotBlank(value)) {
            // Remove the leading and trailing spaces
            value = value.trim();
            checkStringLength(value);
            if (metaField.getMaskingType() != null && value.contains(StringConstant.MASKING_SYMBOL)) {
                // If the masking field contains `****`,
                // throw an exception to prevent from setting the field incorrectly.
                throw new BusinessException("""
                        Model field {0}: {1} is a masking field, and cannot be set including `****`.""",
                        metaField.getModelName(), metaField.getFieldName());
            }
        } else if (AccessType.CREATE.equals(accessType)) {
            checkRequired(row);
            row.computeIfAbsent(fieldName, k -> metaField.getDefaultValueObject());
            return;
        } else if (row.containsKey(fieldName)) {
            // If the field is set to null, check if it is a required field.
            checkRequired(row);
        }
        row.put(fieldName, value);
    }

}
