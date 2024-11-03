package info.openmeta.framework.orm.jdbc.pipeline.factory;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.jdbc.pipeline.processor.BooleanExpandProcessor;
import info.openmeta.framework.orm.jdbc.pipeline.processor.FieldProcessor;
import info.openmeta.framework.orm.jdbc.pipeline.processor.MultiOptionExpandProcessor;
import info.openmeta.framework.orm.jdbc.pipeline.processor.OptionExpandProcessor;
import info.openmeta.framework.orm.meta.MetaField;

/**
 * The output value enhancement for boolean field, single selection field, and multiple selection field.
 * Processed after the computed field is calculated.
 */
public class ExpandProcessorFactory implements FieldProcessorFactory {

    private final ConvertType convertType;

    public ExpandProcessorFactory(ConvertType convertType) {
        this.convertType = convertType;
    }

    /**
     * Create a field processor according to the field metadata.
     *
     * @param metaField field metadata object
     * @param accessType access type
     * @return field processor
     */
    @Override
    public FieldProcessor createProcessor(MetaField metaField, AccessType accessType) {
        FieldType fieldType = metaField.getFieldType();
        if (ConvertType.EXPAND_TYPES.contains(convertType)) {
            if (FieldType.OPTION.equals(fieldType)) {
                // The `OptionExpandProcessor` processor is used for expand cases.
                return new OptionExpandProcessor(metaField, accessType, convertType);
            } else if (FieldType.MULTI_OPTION.equals(fieldType)) {
                return new MultiOptionExpandProcessor(metaField, accessType, convertType);
            }
        }
        // Only for boolean field, the display value is expanded.
        if (ConvertType.DISPLAY.equals(convertType) && FieldType.BOOLEAN.equals(fieldType)) {
            return new BooleanExpandProcessor(metaField, accessType, convertType);
        }
        return null;
    }

}
