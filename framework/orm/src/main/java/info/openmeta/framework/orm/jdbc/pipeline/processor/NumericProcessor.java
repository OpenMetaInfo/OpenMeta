package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.meta.MetaField;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;

/**
 * Numeric field processor.
 * Used to process Integer, Long, Double, BigDecimal fieldTypes.
 */
@Slf4j
public class NumericProcessor extends BaseProcessor {

    private final FieldType fieldType;

    /**
     * NumericProcessor constructor
     *
     * @param metaField field metadata object
     */
    public NumericProcessor(MetaField metaField) {
        super(metaField);
        this.fieldType = metaField.getFieldType();
    }

    /**
     * Formatting of single-row data.
     *
     * @param row Single-row data to be updated
     * @param accessType Access type, such as READ, CREATE, UPDATE
     */
    @Override
    public void processInputRow(Map<String, Object> row, AccessType accessType) {
        checkReadonly(row);
        if (row.containsKey(fieldName) && row.get(fieldName) != null) {
            row.put(fieldName, formatInputNumeric(row.get(fieldName)));
        } else if (AccessType.CREATE.equals(accessType)) {
            checkRequired(row);
            row.put(fieldName, metaField.getDefaultValueObject());
        } else if (row.containsKey(fieldName)){
            checkRequired(row);
            row.put(fieldName, getFieldTypeDefaultValue());
        }
    }

    /**
     * Convert the number type according to the fieldType, such as Integer to Long
     *
     * @param number number object
     * @return converted object
     */
    private Object formatInputNumeric(Object number) {
        if (FieldType.LONG.equals(fieldType) && number instanceof Integer) {
            number = ((Integer) number).longValue();
        } else if (FieldType.DOUBLE.equals(fieldType)) {
            if (number instanceof Integer) {
                number = ((Integer) number).doubleValue();
            } else if (number instanceof Long) {
                number = ((Long) number).doubleValue();
            }
        } else if (!FieldType.BIG_DECIMAL.equals(fieldType) && number instanceof String) {
            throw new IllegalArgumentException("The fieldType of {0} is {1}, but the input value is {2}.",
                    fieldName, fieldType, number);
        }
        return number;
    }
}
