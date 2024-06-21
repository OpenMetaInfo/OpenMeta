package info.openmeta.framework.orm.jdbc.pipeline.factory;

import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.jdbc.pipeline.processor.ManyToManyProcessor;
import info.openmeta.framework.orm.jdbc.pipeline.processor.OneToManyProcessor;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.jdbc.pipeline.processor.FieldProcessor;
import info.openmeta.framework.orm.enums.FieldType;

/**
 * OneToMany/ManyToMany field processor factory.
 */
public class XToManyProcessorFactory implements FieldProcessorFactory {
    private FlexQuery flexQuery;

    public XToManyProcessorFactory() {
    }

    public XToManyProcessorFactory(FlexQuery flexQuery) {
        this.flexQuery = flexQuery;
    }

    /**
     * Create a field processor according to the field metadata.
     *
     * @param metaField field metadata object
     */
    @Override
    public FieldProcessor createProcessor(MetaField metaField) {
        FieldType fieldType = metaField.getFieldType();
        if (FieldType.ONE_TO_MANY.equals(fieldType)) {
            // Process the association table rows according to the ids of the main model.
            return new OneToManyProcessor(metaField, flexQuery);
        } else if (FieldType.MANY_TO_MANY.equals(fieldType)) {
            // Process the middle table rows according to the ids of the main model.
            return new ManyToManyProcessor(metaField, flexQuery);
        }
        return null;
    }

}
