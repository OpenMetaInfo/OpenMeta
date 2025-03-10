package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.meta.MetaField;
import org.apache.commons.lang3.StringUtils;

import java.util.Map;

/**
 * Orders field processor
 */
public class OrdersProcessor extends BaseProcessor {

    public OrdersProcessor(MetaField metaField, AccessType accessType) {
        super(metaField, accessType);
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
     * Convert the Orders object to a string and store it in the database.
     *
     * @param row Single-row data to be created/updated
     */
    @Override
    public void processInputRow(Map<String, Object> row) {
        checkReadonly(row);
        Object value = row.get(fieldName);
        if (value instanceof Orders) {
            row.put(fieldName,  value.toString());
        } else if (value instanceof String) {
            row.put(fieldName, value);
        } else if (AccessType.CREATE.equals(accessType)) {
            checkRequired(row);
            row.computeIfAbsent(fieldName, k -> metaField.getDefaultValueObject());
        }
    }

    /**
     * Convert the string value to an Orders object and replace the original value.
     *
     * @param row Single-row data to be read
     */
    @Override
    public void processOutputRow(Map<String, Object> row) {
        if (!row.containsKey(fieldName)) {
            return;
        }
        Object value = row.get(fieldName);
        value = StringUtils.isBlank((String) value) ? new Orders() : Orders.of((String) value);
        row.put(fieldName, value);
    }

}
