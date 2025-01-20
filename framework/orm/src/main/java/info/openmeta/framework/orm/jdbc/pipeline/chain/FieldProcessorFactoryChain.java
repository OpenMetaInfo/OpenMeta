package info.openmeta.framework.orm.jdbc.pipeline.chain;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.jdbc.pipeline.factory.FieldProcessorFactory;
import info.openmeta.framework.orm.jdbc.pipeline.factory.XToOneGroupProcessorFactory;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Field processor factory chain object. The fieldProcessorFactory need to be added in different orders according to
 * the CREATE, UPDATE, and READ scenarios.
 * The final fieldProcessorChain is generated according to the order of fieldProcessorFactory classes  and the fields
 * to be processed.
 */
public class FieldProcessorFactoryChain {
    private final String modelName;
    private final AccessType accessType;
    private final List<FieldProcessorFactory> processorFactories = new ArrayList<>();

    private FieldProcessorFactoryChain(String modelName, AccessType accessType) {
        this.modelName = modelName;
        this.accessType = accessType;
    }

    /**
     * Static factory chain construction method.
     *
     * @param modelName model name
     * @return factory chain
     */
    public static FieldProcessorFactoryChain of(String modelName, AccessType accessType) {
        return new FieldProcessorFactoryChain(modelName, accessType);
    }

    public FieldProcessorFactoryChain addFactory(FieldProcessorFactory processorFactory) {
        processorFactories.add(processorFactory);
        return this;
    }

    /**
     * Generate the fieldProcessorChain according to the processorFactories and fields.
     *
     * @param fields fields to be processed
     * @return fieldProcessorChain
     */
    public FieldProcessorChain generateProcessorChain(Set<String> fields) {
        FieldProcessorChain fieldProcessorChain = new FieldProcessorChain();
        for (FieldProcessorFactory processorFactory : processorFactories) {
            for (String field : fields) {
                // Custom virtual cascaded field doesn't exist in metadata, so create a metaField object here
                // for processing.
                // TODO: Need to use other solutions to replace this.
                if (field.contains(".") && processorFactory instanceof XToOneGroupProcessorFactory) {
                    MetaField metaField = this.createCustomCascadedField(field);
                    fieldProcessorChain.addProcessor(processorFactory.createProcessor(metaField, accessType));
                } else if (!field.contains(".")){
                    MetaField metaField = ModelManager.getModelField(modelName, field);
                    fieldProcessorChain.addProcessor(processorFactory.createProcessor(metaField, accessType));
                }
            }
        }
        return fieldProcessorChain;
    }

    /**
     * Initialize the custom virtual cascaded fields.
     *
     * @param fieldName custom cascaded field name like `a.b.c`
     */
    @Deprecated
    private MetaField createCustomCascadedField(String fieldName) {
        String[] fieldsArray = StringUtils.split(fieldName, ".");
        Assert.isTrue(fieldsArray.length - 1 <= BaseConstant.CASCADE_LEVEL, """
                        Due to database performance considerations, obtain the model cascaded field {0}: {1},
                        the cascade level cannot exceed the {2} level.""",
                modelName, fieldName, BaseConstant.CASCADE_LEVEL);
        MetaField customCascadedField = new MetaField();
        customCascadedField.setModelName(modelName);
        customCascadedField.setFieldName(fieldName);
        customCascadedField.setCascadedField(fieldName);
        customCascadedField.setDynamic(true);
        // Use the type of the last field as the actual value type.
        MetaField lastField = ModelManager.getLastFieldOfCascaded(modelName, fieldName);
        customCascadedField.setFieldType(lastField.getFieldType());
        return customCascadedField;
    }
}
