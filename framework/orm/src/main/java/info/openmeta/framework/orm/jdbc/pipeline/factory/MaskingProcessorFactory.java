package info.openmeta.framework.orm.jdbc.pipeline.factory;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.jdbc.pipeline.processor.FieldProcessor;
import info.openmeta.framework.orm.jdbc.pipeline.processor.MaskingProcessor;
import info.openmeta.framework.orm.meta.MetaField;

/**
 * Masking field processor factory.
 * Used only in output data processing.
 */
public class MaskingProcessorFactory implements FieldProcessorFactory {

    /**
     * Create a field processor according to the field metadata.
     *
     * @param metaField field metadata object
     * @param accessType access type
     * @return masking processor
     */
    @Override
    public FieldProcessor createProcessor(MetaField metaField, AccessType accessType) {
        FieldType fieldType = metaField.getFieldType();
        if (FieldType.STRING.equals(fieldType) && metaField.getMaskingType() != null) {
            return new MaskingProcessor(metaField, accessType);
        }
        return null;
    }

}
