package info.openmeta.framework.orm.jdbc.pipeline.factory;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.jdbc.pipeline.processor.*;
import info.openmeta.framework.orm.meta.MetaField;
import lombok.NoArgsConstructor;

/**
 * Type cast processor factory.
 * Process the field type conversion for Option, List, Json, Filter, etc.
 * The value can be used in calculation after convert.
 */
@NoArgsConstructor
public class TypeCastProcessorFactory implements FieldProcessorFactory {

    private ConvertType convertType;

    public TypeCastProcessorFactory(ConvertType convertType) {
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
        if (FieldType.JSON.equals(fieldType)) {
            return new JsonProcessor(metaField, accessType);
        } else if (FieldType.MULTI_STRING.equals(fieldType)) {
            return new MultiStringProcessor(metaField, accessType, convertType);
        } else if (FieldType.MULTI_OPTION.equals(fieldType)) {
            // During type conversion for MULTI_OPTION fields, using `MultiStringProcessor` processor
            // to convert the field value between List and String object.
            // But in the expand case, using `MultiOptionProcessor` processor to expand the field value.
            return new MultiStringProcessor(metaField, accessType, convertType);
        } else if (FieldType.FILTERS.equals(fieldType)) {
            return new FiltersProcessor(metaField, accessType);
        } else if (FieldType.ORDERS.equals(fieldType)) {
            return new OrdersProcessor(metaField, accessType);
        }
        return null;
    }

}
