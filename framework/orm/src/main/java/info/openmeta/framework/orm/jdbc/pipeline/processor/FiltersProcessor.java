package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.meta.MetaField;
import org.apache.commons.lang3.StringUtils;

import java.util.Map;

/**
 * Filter field processor
 */
public class FiltersProcessor extends BaseProcessor {

    public FiltersProcessor(MetaField metaField) {
        super(metaField);
    }

    /**
     * Check if the required field is assigned and is a non-empty string, non-null
     */
    @Override
    protected void checkRequired(Map<String, Object> row) {
        if (metaField.isRequired() && StringUtils.isBlank((String) row.get(fieldName))) {
            throw new IllegalArgumentException("Model field {0}:{1} value cannot be empty!", metaField.getModelName(), fieldName);
        }
    }

    /**
     * Convert the Filters object to a string and store it in the database.
     *
     * @param row Single-row data to be created/updated
     * @param accessType Access type
     */
    @Override
    public void processInputRow(Map<String, Object> row, AccessType accessType) {
        checkReadonly(row);
        Object value = row.get(fieldName);
        if (value instanceof Filters) {
            row.put(fieldName,  value.toString());
        } else if (value instanceof String) {
            row.put(fieldName, value);
        } else if (AccessType.CREATE.equals(accessType) || row.containsKey(fieldName)) {
            checkRequired(row);
            row.put(fieldName, "");
        }
    }

    /**
     * Convert the string value to a Filters object and replace the original value.
     *
     * @param row Single-row data to be read
     */
    @Override
    public void processOutputRow(Map<String, Object> row) {
        if (!row.containsKey(fieldName)) {
            return;
        }
        Object value = row.get(fieldName);
        value = StringUtils.isBlank((String) value) ? new Filters() : Filters.of((String) value);
        row.put(fieldName, value);
    }

}
