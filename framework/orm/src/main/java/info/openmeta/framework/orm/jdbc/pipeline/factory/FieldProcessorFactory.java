package info.openmeta.framework.orm.jdbc.pipeline.factory;

import info.openmeta.framework.orm.jdbc.pipeline.processor.FieldProcessor;
import info.openmeta.framework.orm.meta.MetaField;

/**
 * Field processor factory interface
 */
public interface FieldProcessorFactory {

    /**
     * Create a field processor according to the field metadata.
     *
     * @param metaField field metadata object
     */
    FieldProcessor createProcessor(MetaField metaField);

}
