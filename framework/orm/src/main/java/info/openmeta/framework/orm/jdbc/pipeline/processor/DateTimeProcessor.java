package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.constant.TimeConstant;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.utils.DateUtils;
import info.openmeta.framework.orm.meta.MetaField;

import java.time.LocalDateTime;
import java.util.Map;

/**
 * DateTime field processor.
 * Both input and output are formatted to LocalDateTime for DateTime type.
 * When globally serialized, use MappingJackson2HttpMessageConverter to convert to a string output.
 */
public class DateTimeProcessor extends BaseProcessor {

    /**
     * Field processor object constructor
     *
     * @param metaField field metadata object
     */
    public DateTimeProcessor(MetaField metaField) {
        super(metaField);
    }

    /**
     * Single-row data formatting processing.
     *
     * @param row Single-row data to be created/updated
     * @param accessType Access type, such as READ, CREATE, UPDATE
     */
    public void processInputRow(Map<String, Object> row, AccessType accessType) {
        checkReadonly(row);
        if (AccessType.CREATE.equals(accessType)) {
            checkRequired(row);
            if (TimeConstant.NOW.equalsIgnoreCase(metaField.getDefaultValue())) {
                // When creating, assign the current time default value.
                row.computeIfAbsent(fieldName, k -> LocalDateTime.now());
            }
        } else if (row.containsKey(fieldName) && row.get(fieldName) == null) {
            // Check if the required field is set to null.
            checkRequired(row);
        }
    }

    /**
     * Convert the DateTime object to LocalDateTime, and compatible with LocalDateTime, Date, and null.
     *
     * @param row Single-row output data
     */
    @Override
    public void processOutputRow(Map<String, Object> row) {
        if (!row.containsKey(fieldName)) {
            return;
        }
        LocalDateTime dateTime = DateUtils.dateToLocalDateTime(row.get(fieldName));
        row.put(fieldName, dateTime);
    }
}
