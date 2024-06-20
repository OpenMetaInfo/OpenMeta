package info.openmeta.framework.orm.jdbc.pipeline.processor;

import com.fasterxml.jackson.databind.JsonNode;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.exception.JSONException;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.orm.meta.MetaField;
import org.apache.commons.lang3.StringUtils;

import java.util.Map;

/**
 * JSON field processor
 */
public class JsonProcessor extends BaseProcessor {

    public JsonProcessor(MetaField metaField) {
        super(metaField);
    }

    /**
     * Convert the JSON object to a string and store it in the database.
     *
     * @param row Single-row data to be created/updated
     * @param accessType Access type
     */
    @Override
    public void processInputRow(Map<String, Object> row, AccessType accessType) {
        checkReadonly(row);
        Object value = row.get(fieldName);
        if (value != null) {
            // If the value is already a string, no processing is required
            if (!(value instanceof String)) {
                row.put(fieldName, JsonMapper.objectToString(value));
            }
        } else if (AccessType.CREATE.equals(accessType)) {
            checkRequired(row);
            row.put(fieldName, metaField.getDefaultValueObject());
        } else if (row.containsKey(fieldName)) {
            // The field is set to null, check if it is a required field.
            checkRequired(row);
            row.put(fieldName, getFieldTypeDefaultValue());
        }
    }

    /**
     * Convert the string value to a JsonNode object and replace the original value.
     *
     * @param row Single-row data to be read
     */
    @Override
    public void processOutputRow(Map<String, Object> row) {
        if (!row.containsKey(fieldName) || !(row.get(fieldName) instanceof String)) {
            return;
        }
        try {
            String value = (String) row.get(fieldName);
            if (StringUtils.isBlank(value)) {
                row.put(fieldName, getFieldTypeDefaultValue());
            } else {
                row.put(fieldName, JsonMapper.stringToObject(value, JsonNode.class));
            }
        } catch (JSONException e) {
            throw new IllegalArgumentException("The value of field {0}: {1} is not a valid JSON string: {2}.",
                    fieldName, row.get(fieldName), e.getMessage());
        } catch (ClassCastException e) {
            throw new IllegalArgumentException(e.getMessage());
        }
    }
}
