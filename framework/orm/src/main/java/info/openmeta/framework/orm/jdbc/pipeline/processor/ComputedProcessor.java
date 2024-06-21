package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.utils.DateUtils;
import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import org.apache.commons.lang3.StringUtils;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static info.openmeta.framework.orm.enums.FieldType.*;

/**
 * Computed field processor
 */
public class ComputedProcessor extends BaseProcessor {

    /**
     * Field processor object constructor
     *
     * @param metaField field metadata object
     */
    public ComputedProcessor(MetaField metaField) {
        super(metaField);
    }

    /**
     * Execute the calculation of the computed field for the input row.
     *
     * @param row      Single-row data to be created/updated
     * @param accessType Access type, such as READ, CREATE, UPDATE
     */
    @Override
    public void processInputRow(Map<String, Object> row, AccessType accessType) {
        executeCompute(row);
    }

    /**
     * Execute the calculation of the computed field for the output row.
     *
     * @param row Single-row data to be read
     */
    @Override
    public void processOutputRow(Map<String, Object> row) {
        executeCompute(row);
    }

    /**
     * Execute the calculation of the computed field.
     *
     * @param row Single-row data
     */
    public void executeCompute(Map<String, Object> row) {
        Map<String, Object> env = new HashMap<>();
        metaField.getDependentFields().forEach(field -> env.put(field, formatComputeEnv(field, row.get(field))));
        Object result = ComputeUtils.execute(metaField.getExpression(), env, metaField.getScale(), metaField.getFieldType());
        row.put(fieldName, result);
    }

    /**
     * Format the environment variable of the calculation formula. Such as:
     * convert the number to BigDecimal, and convert the date to LocalDate or LocalDateTime.
     *
     * @param field field name
     * @param value field value
     * @return Formatted field value
     */
    private Object formatComputeEnv(String field, Object value) {
        FieldType fieldType = ModelManager.getModelField(metaField.getModelName(), field).getFieldType();
        Object result;
        if (NUMERIC_TYPES.contains(fieldType) && !(value instanceof BigDecimal)) {
            // If the fieldType is numeric, convert the value to BigDecimal, and the return value is BigDecimal.
            result = value == null ? new BigDecimal("0") : new BigDecimal(String.valueOf(value));
        } else if (fieldType.equals(DATE) && !(value instanceof LocalDate)) {
            // Convert Date type to LocalDate object.
            result = DateUtils.dateToLocalDate(value);
        } else if (fieldType.equals(DATE_TIME) && !(value instanceof LocalDateTime)) {
            // Convert DateTime type to LocalDateTime object.
            result = DateUtils.dateToLocalDateTime(value);
        } else if ((fieldType.equals(MULTI_STRING) || fieldType.equals(MULTI_OPTION) && value instanceof String)) {
            // Convert the string value to a list when the fieldType is MULTI_STRING or MULTI_OPTION.
            result = Arrays.asList(StringUtils.split((String) value, ","));
        } else {
            result = value;
        }
        return result;
    }

}
