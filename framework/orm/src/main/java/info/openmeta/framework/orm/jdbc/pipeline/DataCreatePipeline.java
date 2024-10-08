package info.openmeta.framework.orm.jdbc.pipeline;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.enums.IdStrategy;
import info.openmeta.framework.orm.jdbc.AutofillFields;
import info.openmeta.framework.orm.jdbc.pipeline.chain.FieldProcessorChain;
import info.openmeta.framework.orm.jdbc.pipeline.chain.FieldProcessorFactoryChain;
import info.openmeta.framework.orm.jdbc.pipeline.factory.*;
import info.openmeta.framework.orm.meta.ModelManager;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Pipeline for creating model data.
 * Including batch encryption, data format conversion, default value assignment, computed fields calculation,
 * and length, required, read-only and other attribute verification.
 * <p>
 * When processing the associated OneToMany, ManyToMany fields, without permission check temporarily.
 */
@Slf4j
@Getter
public class DataCreatePipeline extends DataPipeline {

    private static final AccessType accessType = AccessType.CREATE;

    /** The set of stored fields */
    protected Set<String> storedFields;

    /**
     * Build the CREATE pipeline.
     * All stored fields are assigned specified values or default values.
     *
     * @param modelName model name
     */
    public DataCreatePipeline(String modelName) {
        super(modelName);
        this.fields = ModelManager.getModelUpdatableFields(modelName);
        this.addEffectedFields();
        // If the IdStrategy is not DB_AUTO_ID, add the ID field to the list of fields to be processed.
        if (!IdStrategy.DB_AUTO_ID.equals(ModelManager.getIdStrategy(modelName))) {
            this.fields.add(ModelManager.getModelPrimaryKey(modelName));
        }
        this.processorChain = buildFieldProcessorChain();
    }

    /**
     * Add the stored cascaded fields and stored computed fields.
     */
    private void addEffectedFields() {
        ModelManager.getModelCascadedFields(modelName, false).forEach(metaField -> this.fields.add(metaField.getFieldName()));
        ModelManager.getModelComputedFields(modelName, false).forEach(metaField -> this.fields.add(metaField.getFieldName()));
    }

    /**
     * Create processor factory chain according to the data processing order of the creation scenario,
     * and generate the final field processing responsibility chain `FieldProcessorChain`.
     */
    @Override
    public FieldProcessorChain buildFieldProcessorChain() {
        FieldProcessorFactoryChain factoryChain = FieldProcessorFactoryChain.of(modelName)
                .addFactory(new XToOneGroupProcessorFactory(accessType))
                .addFactory(new NormalProcessorFactory())
                .addFactory(new ComputeProcessorFactory(accessType))
                .addFactory(new TypeCastProcessorFactory())
                // Encrypt the `encrypted` fields before storing them in the database.
                .addFactory(new EncryptProcessorFactory());
        return factoryChain.generateProcessorChain(fields);
    }

    /**
     * Process the rows data of Create.
     *
     * @param rows rows
     * @param createdTime creation time
     */
    @Override
    public List<Map<String, Object>> processCreateData(List<Map<String, Object>> rows, LocalDateTime createdTime) {
        // Format the field data of the current model
        processorChain.processInputRows(rows, accessType);
        // Fill in the audit fields
        AutofillFields.fillAuditFieldsForInsert(rows, createdTime);
        // Extract the set of stored fields
        this.storedFields = fields.stream().filter(field -> ModelManager.isStored(modelName, field)).collect(Collectors.toSet());
        this.storedFields.addAll(ModelConstant.AUDIT_FIELDS);
        // Fill in the tenant field for multi-tenant models
        if (ModelManager.isMultiTenant(modelName)) {
            AutofillFields.fillTenantFieldForInsert(rows);
            this.storedFields.add(ModelConstant.TENANT_ID);
        }
        return rows;
    }

    /**
     * After obtaining the ids, process the OneToMany and ManyToMany fields of current model.
     *
     * @param rows rows
     */
    @Override
    public void processXToManyData(List<Map<String, Object>> rows) {
        FieldProcessorFactoryChain xToManyFactoryChain = FieldProcessorFactoryChain.of(modelName)
                .addFactory(new XToManyProcessorFactory());
        xToManyFactoryChain.generateProcessorChain(fields).processInputRows(rows, accessType);
    }
}
