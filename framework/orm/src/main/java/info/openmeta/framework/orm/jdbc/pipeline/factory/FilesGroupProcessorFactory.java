package info.openmeta.framework.orm.jdbc.pipeline.factory;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.jdbc.pipeline.processor.FieldProcessor;
import info.openmeta.framework.orm.jdbc.pipeline.processor.FilesGroupProcessor;
import info.openmeta.framework.orm.meta.MetaField;

/**
 * The processor factory of creating FilesGroupProcessor, which is processing File fields and MultiFile fields.
 */
public class FilesGroupProcessorFactory implements FieldProcessorFactory {

    // the FilesGroupProcessor object to process File fields and MultiFile fields
    private FilesGroupProcessor filesGroupProcessor;

    public FilesGroupProcessorFactory() {}

    /**
     * Create a field processor according to the field metadata.
     *
     * @param metaField field metadata object
     * @param accessType access type
     */
    @Override
    public FieldProcessor createProcessor(MetaField metaField, AccessType accessType) {
        FieldType fieldType = metaField.getFieldType();
        if (AccessType.READ.equals(accessType) && FieldType.FILE_TYPES.contains(fieldType)) {
            if (this.filesGroupProcessor == null) {
                this.filesGroupProcessor = new FilesGroupProcessor(metaField, accessType);
                return this.filesGroupProcessor;
            } else {
                this.filesGroupProcessor.addFileField(metaField);
            }
        }
        return null;
    }

}
