package info.openmeta.framework.orm.jdbc.pipeline.factory;

import info.openmeta.framework.orm.jdbc.pipeline.processor.ComputedProcessor;
import info.openmeta.framework.orm.jdbc.pipeline.processor.FieldProcessor;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.base.enums.AccessType;

/**
 * Compute field processor factory.
 * The value type of the computed field, can be a numeric field, a date field, an option set, etc.
 * Therefore, in addition to the computed processor, a processor corresponding to the data type will be created.
 */
public class ComputeProcessorFactory implements FieldProcessorFactory {

    /**
     * Create a field processor according to the field metadata.
     * READ scenario: only dynamic computed fields are calculated.
     * CREATE or UPDATE scenario: only stored computed fields are calculated.
     *
     * @param metaField field object
     * @param accessType access type
     * @return computed processor
     */
    @Override
    public FieldProcessor createProcessor(MetaField metaField, AccessType accessType) {
        if (metaField.isComputed()) {
            if (AccessType.READ.equals(accessType) && metaField.isDynamic()) {
                return new ComputedProcessor(metaField, accessType);
            } else if (!AccessType.READ.equals(accessType) && !metaField.isDynamic()) {
                return new ComputedProcessor(metaField, accessType);
            }
        }
        return null;
    }

}
