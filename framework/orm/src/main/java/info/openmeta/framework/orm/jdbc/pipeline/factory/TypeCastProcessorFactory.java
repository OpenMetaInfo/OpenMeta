package info.openmeta.framework.orm.jdbc.pipeline.factory;

import info.openmeta.framework.orm.jdbc.pipeline.processor.*;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.enums.FieldType;

/**
 * Type cast processor factory.
 * Process the field type conversion for Option, List, Json, Filter, etc.
 * The value can be used in calculation after convert.
 */
public class TypeCastProcessorFactory implements FieldProcessorFactory {

    /**
     * Create a field processor according to the field metadata.
     *
     * @param metaField field metadata object
     */
    @Override
    public FieldProcessor createProcessor(MetaField metaField) {
        FieldType fieldType = metaField.getFieldType();
        if (FieldType.JSON.equals(fieldType)) {
            return new JsonProcessor(metaField);
        } else if (FieldType.MULTI_STRING.equals(fieldType)) {
            return new MultiStringProcessor(metaField);
        } else if (FieldType.MULTI_OPTION.equals(fieldType)) {
            // During type conversion for MULTI_OPTION fields, using `MultiStringProcessor` processor
            // to convert the field value between List and String object.
            // But in the expand case, using `MultiOptionProcessor` processor to expand the field value.
            return new MultiStringProcessor(metaField);
        } else if (FieldType.FILTERS.equals(fieldType)) {
            return new FiltersProcessor(metaField);
        } else if (FieldType.ORDERS.equals(fieldType)) {
            return new OrdersProcessor(metaField);
        }
        return null;
    }

}
