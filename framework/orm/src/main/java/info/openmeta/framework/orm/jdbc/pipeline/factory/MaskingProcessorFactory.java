package info.openmeta.framework.orm.jdbc.pipeline.factory;

import info.openmeta.framework.orm.jdbc.pipeline.processor.MaskingProcessor;
import info.openmeta.framework.orm.jdbc.pipeline.processor.FieldProcessor;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.enums.FieldType;

/**
 * Masking field processor factory.
 * Used only in output data processing.
 */
public class MaskingProcessorFactory implements FieldProcessorFactory {

    /**
     * Create a field processor according to the field metadata.
     *
     * @param metaField field metadata object
     */
    @Override
    public FieldProcessor createProcessor(MetaField metaField) {
        FieldType fieldType = metaField.getFieldType();
        if (FieldType.STRING.equals(fieldType) && metaField.getMaskingType() != null) {
            return new MaskingProcessor(metaField);
        }
        return null;
    }

}
