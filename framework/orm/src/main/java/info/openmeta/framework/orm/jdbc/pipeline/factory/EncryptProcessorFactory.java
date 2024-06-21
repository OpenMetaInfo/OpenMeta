package info.openmeta.framework.orm.jdbc.pipeline.factory;

import info.openmeta.framework.orm.jdbc.pipeline.processor.EncryptedProcessor;
import info.openmeta.framework.orm.jdbc.pipeline.processor.FieldProcessor;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.enums.FieldType;

/**
 * Encrypted field processor factory, encrypt before storage and decrypt when output.
 */
public class EncryptProcessorFactory implements FieldProcessorFactory {

    /**
     * Create a field processor according to the field metadata.
     *
     * @param metaField field metadata object
     */
    @Override
    public FieldProcessor createProcessor(MetaField metaField) {
        FieldType fieldType = metaField.getFieldType();
        if (FieldType.STRING.equals(fieldType) && (metaField.getEncrypted())) {
            return new EncryptedProcessor(metaField);
        }
        return null;
    }

}
