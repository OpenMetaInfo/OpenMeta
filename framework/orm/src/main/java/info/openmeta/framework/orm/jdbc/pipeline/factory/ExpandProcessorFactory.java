package info.openmeta.framework.orm.jdbc.pipeline.factory;

import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.jdbc.pipeline.processor.BooleanExpandProcessor;
import info.openmeta.framework.orm.jdbc.pipeline.processor.FieldProcessor;
import info.openmeta.framework.orm.jdbc.pipeline.processor.MultiOptionProcessor;
import info.openmeta.framework.orm.jdbc.pipeline.processor.OptionProcessor;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.enums.FieldType;

/**
 * The output value enhancement for boolean field, single selection field, and multiple selection field.
 * Processed after the computed field is calculated.
 */
public class ExpandProcessorFactory implements FieldProcessorFactory {

    private final ConvertType convertType;

    public ExpandProcessorFactory(FlexQuery flexQuery) {
        this.convertType = flexQuery.getConvertType();
    }

    /**
     * Create a field processor according to the field metadata.
     *
     * @param metaField field metadata object
     */
    @Override
    public FieldProcessor createProcessor(MetaField metaField) {
        FieldType fieldType = metaField.getFieldType();
        if (ConvertType.EXPAND_TYPES.contains(convertType)) {
            if (FieldType.OPTION.equals(fieldType)) {
                // The `OptionProcessor` processor is used for expand cases.
                return new OptionProcessor(metaField, convertType);
            } else if (FieldType.MULTI_OPTION.equals(fieldType)) {
                return new MultiOptionProcessor(metaField, convertType);
            } else if (FieldType.BOOLEAN.equals(fieldType)) {
                return new BooleanExpandProcessor(metaField, convertType);
            }
        }
        return null;
    }

}
