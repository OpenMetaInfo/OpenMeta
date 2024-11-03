package info.openmeta.framework.orm.jdbc.pipeline.factory;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.jdbc.pipeline.processor.FieldProcessor;
import info.openmeta.framework.orm.jdbc.pipeline.processor.XToOneGroupProcessor;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import org.apache.commons.lang3.StringUtils;

import java.util.HashMap;
import java.util.Map;

/**
 * The processor factory of creating XToOneGroupProcessor, which is processing
 * OneToOne/ManyToOne field and the cascaded fields that depend on them as a group.
 * The cascaded field depends on a OneToOne/ManyToOne field, they are processed together for input and output data.
 */
public class XToOneGroupProcessorFactory implements FieldProcessorFactory {

    private FlexQuery flexQuery;

    // THe mapping of ManyToOne/OneToOne fieldName to XToOneGroupProcessor
    protected final Map<String, XToOneGroupProcessor> relatedFieldMap = new HashMap<>();

    public XToOneGroupProcessorFactory() {}

    public XToOneGroupProcessorFactory(FlexQuery flexQuery) {
        this.flexQuery = flexQuery;
    }

    /**
     * Create a field processor according to the field metadata.
     *
     * @param metaField field metadata object
     * @param accessType access type
     */
    @Override
    public FieldProcessor createProcessor(MetaField metaField, AccessType accessType) {
        FieldType fieldType = metaField.getFieldType();
        if (AccessType.READ.equals(accessType)) {
            if (StringUtils.isNotBlank(metaField.getCascadedField()) && metaField.isDynamic()) {
                // READ scenario: calculate dynamic cascaded field
                return this.updateRelatedFields(metaField, accessType);
            } else if (FieldType.TO_ONE_TYPES.contains(fieldType) && ConvertType.EXPAND_TYPES.contains(flexQuery.getConvertType())) {
                if (metaField.getFieldName().equals(flexQuery.getKeepIdField())) {
                    // When READ OneToMany field, don't enhance the ManyToOne field of the associated model.
                    return null;
                }
                // Create the XToOneGroupProcessor when the field group is not created by the cascaded field.
                if (!this.relatedFieldMap.containsKey(metaField.getFieldName())) {
                    XToOneGroupProcessor xToOneGroupProcessor = new XToOneGroupProcessor(metaField, accessType, flexQuery);
                    this.relatedFieldMap.put(metaField.getFieldName(), xToOneGroupProcessor);
                    return xToOneGroupProcessor;
                }
            }
        } else if (StringUtils.isNotBlank(metaField.getCascadedField()) && !metaField.isDynamic()) {
            // CREATE/UPDATE scenario: calculate stored cascaded field
            return this.updateRelatedFields(metaField, accessType);
        }
        return null;
    }

    /**
     * Create or update XToOneGroupProcessor, bind XToOne field, cascaded fields that depend on this field,
     * and the expand fields of the associated model based on the ManyToOne/OneToOne field.
     *
     * @param cascadedField cascaded field metadata object
     * @param accessType    access type
     * @return XToOneGroupProcessor
     */
    private XToOneGroupProcessor updateRelatedFields(MetaField cascadedField, AccessType accessType) {
        String[] fieldsArray = StringUtils.split(cascadedField.getCascadedField(), ".", 2);
        String xToOneFieldName = fieldsArray[0];
        if (this.relatedFieldMap.containsKey(xToOneFieldName)) {
            // Add the cascaded field to the existing XToOneGroupProcessor
            XToOneGroupProcessor xToOneGroupProcessor = this.relatedFieldMap.get(xToOneFieldName);
            xToOneGroupProcessor.addCascadedField(cascadedField);
        } else {
            // Create the XToOneGroupProcessor for the first time
            MetaField xToOneField = ModelManager.getModelField(cascadedField.getModelName(), xToOneFieldName);
            XToOneGroupProcessor xToOneGroupProcessor = new XToOneGroupProcessor(xToOneField, accessType, flexQuery);
            xToOneGroupProcessor.addCascadedField(cascadedField);
            this.relatedFieldMap.put(xToOneFieldName, xToOneGroupProcessor);
            return xToOneGroupProcessor;
        }
        return null;
    }

}
