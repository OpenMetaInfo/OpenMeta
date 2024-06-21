package info.openmeta.framework.orm.jdbc.pipeline.factory;

import info.openmeta.framework.orm.jdbc.pipeline.processor.*;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.enums.FieldType;

/**
 * Normal field processor factory.
 * Check required, readonly, length, etc. and assign default values.
 */
public class NormalProcessorFactory implements FieldProcessorFactory {

    /**
     * Create a field processor according to the field metadata.
     *
     * @param metaField field metadata object
     */
    @Override
    public FieldProcessor createProcessor(MetaField metaField) {
        FieldType fieldType = metaField.getFieldType();
        if (FieldType.STRING.equals(fieldType)) {
            return new StringProcessor(metaField);
        } else if (FieldType.BOOLEAN.equals(fieldType)) {
            return new BooleanProcessor(metaField);
        } else if (FieldType.OPTION.equals(fieldType)) {
            // In normal processing, OPTION field is processed as a string with default values,
            // and the `OptionProcessor` processor is used for expand cases.
            return new StringProcessor(metaField);
        } else if (FieldType.NUMERIC_TYPES.contains(fieldType)) {
            return new NumericProcessor(metaField);
        } else if (FieldType.DATE.equals(fieldType)) {
            return new DateProcessor(metaField);
        } else if (FieldType.DATE_TIME.equals(fieldType)) {
            return new DateTimeProcessor(metaField);
        }
        return null;
    }

}
