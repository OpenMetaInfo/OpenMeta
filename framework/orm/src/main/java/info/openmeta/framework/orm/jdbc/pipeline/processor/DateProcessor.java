package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.constant.TimeConstant;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.utils.DateUtils;
import info.openmeta.framework.orm.meta.MetaField;

import java.time.LocalDate;
import java.util.Map;

/**
 * Date field processor.
 * Both input and output are formatted to LocalDate for DATE type.
 * When globally serialized, use MappingJackson2HttpMessageConverter to convert to a string output.
 */
public class DateProcessor extends BaseProcessor {

    /**
     * Field processor object constructor
     *
     * @param metaField field metadata object
     * @param accessType access type
     */
    public DateProcessor(MetaField metaField, AccessType accessType) {
        super(metaField, accessType);
    }

    /**
     * Single-row data formatting processing.
     *
     * @param row Single-row data to be created/updated
     */
    public void processInputRow(Map<String, Object> row) {
        checkReadonly(row);
        if (AccessType.CREATE.equals(accessType)) {
            checkRequired(row);
            row.computeIfAbsent(fieldName, k -> {
                if (TimeConstant.NOW.equals(metaField.getDefaultValueObject())) {
                    // Assign the current time as the default value.
                    return LocalDate.now();
                } else {
                    return metaField.getDefaultValueObject();
                }
            });
        } else if (row.containsKey(fieldName) && row.get(fieldName) == null) {
            // UPDATE: Check if the required field is set to null.
            checkRequired(row);
        }
    }

    /**
     * Convert the date object output to LocalDate, compatible with LocalDate, Date, null.
     *
     * @param row Single-row output data
     */
    @Override
    public void processOutputRow(Map<String, Object> row) {
        if (!row.containsKey(fieldName)) {
            return;
        }
        LocalDate date = DateUtils.dateToLocalDate(row.get(fieldName));
        row.put(fieldName, date);
    }
}
