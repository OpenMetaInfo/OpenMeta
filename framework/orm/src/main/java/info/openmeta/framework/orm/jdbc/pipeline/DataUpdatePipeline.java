package info.openmeta.framework.orm.jdbc.pipeline;

import info.openmeta.framework.base.constant.TimeConstant;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.exception.VersionException;
import info.openmeta.framework.base.utils.DateUtils;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.jdbc.AutofillFields;
import info.openmeta.framework.orm.jdbc.pipeline.chain.FieldProcessorChain;
import info.openmeta.framework.orm.jdbc.pipeline.chain.FieldProcessorFactoryChain;
import info.openmeta.framework.orm.jdbc.pipeline.factory.*;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

/**
 * Data processing pipeline for model data UPDATE, completing the inspection and conversion of model input data.
 * Including batch encryption, data format conversion, default value assignment,
 * and length, required, read-only, etc. attribute verification.
 * When processing related OneToMany, ManyToMany fields, without permission check temporarily.
 */
@Slf4j
public class DataUpdatePipeline extends DataPipeline {

    private final String primaryKey;

    /**
     * The fields that possibly to be updated.
     * If the field value is the same as the database value, it is ignored.
     */
    @Getter
    protected Set<String> fields;

    /**
     * The fields that need to be compared with database value before updated.
     * Including the fields that can be updated by the API, and the fields
     * that are automatically cascaded and calculated by the program.
     * But excluding the XToMany fields.
     */
    @Getter
    private final Set<String> differFields = new HashSet<>();

    private static final AccessType accessType = AccessType.UPDATE;

    /**
     * Build the UPDATE pipeline and data processor chain.
     * The processor chain is generated based on the fields that possibly to be updated.
     *
     * @param modelName model name
     * @param fields fields possibly to be updated
     */
    public DataUpdatePipeline(String modelName, Set<String> fields) {
        super(modelName);
        this.primaryKey = ModelManager.getModelPrimaryKey(modelName);
        this.fields = fields;
        // Add the affected cascaded fields and computed fields to this.fields
        this.updateEffectedFields();
        this.updateDifferFields();
        this.processorChain = buildFieldProcessorChain();
    }

    /**
     * Build the field processing chain according to the data processing order of the UPDATE scenario,
     * and generate the final field processing responsibility chain `FieldProcessorChain`.
     */
    @Override
    public FieldProcessorChain buildFieldProcessorChain() {
        FieldProcessorFactoryChain factoryChain = FieldProcessorFactoryChain.of(modelName)
                .addFactory(new XToOneGroupProcessorFactory(accessType))
                .addFactory(new NormalProcessorFactory())
                .addFactory(new ComputeProcessorFactory(accessType))
                .addFactory(new TypeCastProcessorFactory())
                .addFactory(new EncryptProcessorFactory());
        return factoryChain.generateProcessorChain(fields);
    }

    /**
     * Process the rows data of Update.
     * Get the rows that have been modified, and fill in the audit fields.
     *
     * @param rows rows data
     * @param originalRowsMap original rows data from the database
     * @param updatedTime update time
     * @return processed data
     */
    @Override
    public List<Map<String, Object>> processUpdateData(List<Map<String, Object>> rows, Map<Serializable, Map<String, Object>> originalRowsMap, LocalDateTime updatedTime) {
        List<Map<String, Object>> mergedRows = mergeToOriginalData(rows, originalRowsMap);
        processorChain.processInputRows(mergedRows, accessType);
        // TODO: Compare the encrypted fields using plaintext to be compatible with different encryption algorithms,
        //  to avoid the situation where the plaintext is the same but the ciphertext is different.
        List<Map<String, Object>> differRows = differFromPrevious(modelName, primaryKey, this.differFields, mergedRows, originalRowsMap);
        if (!differRows.isEmpty()) {
            AutofillFields.fillAuditFieldsForUpdate(differRows, updatedTime);
        }
        return differRows;
    }

    /**
     * After obtaining the ids of main model, process the OneToMany and ManyToMany fields.
     * Temporarily without permission check.
     */
    @Override
    public void processXToManyData(List<Map<String, Object>> rows) {
        FieldProcessorFactoryChain xToManyFactoryChain = FieldProcessorFactoryChain.of(modelName)
                .addFactory(new XToManyProcessorFactory());
        xToManyFactoryChain.generateProcessorChain(fields).processInputRows(rows, accessType);
    }

    /**
     * Determine the affected stored cascaded fields and stored computed fields,
     * and add them to the list of fields to be processed.
     */
    private void updateEffectedFields() {
        for (MetaField metaField : ModelManager.getModelCascadedFields(modelName, false)) {
            // For cascaded field, re-cascade it when the dependent ManyToOne field is modified.
            String xToOneFieldName = StringUtils.split(metaField.getCascadedField(), ".")[0];
            if (this.fields.contains(xToOneFieldName)) {
                this.fields.add(metaField.getFieldName());
            }
        }
        for (MetaField metaField : ModelManager.getModelComputedFields(modelName, false)) {
            // For computed field, re-calculate it when any of the dependent fields are modified.
            Set<String> dependentFields = new HashSet<>(metaField.getDependentFields());
            dependentFields.retainAll(this.fields);
            if (!dependentFields.isEmpty()) {
                this.fields.add(metaField.getFieldName());
                // The dependent fields are not automatically handled by the processor,
                // but need to get their old values in database.
                this.differFields.addAll(metaField.getDependentFields());
            }
        }
    }

    /**
     * Add the fields to be updated to the differFields set, used to generate ChangeLogs.
     * XToMany fields are processed separately for updates and have their own ChangeLog rows.
     */
    private void updateDifferFields() {
        this.fields.stream().filter(field -> !FieldType.TO_MANY_TYPES.contains(ModelManager.getModelField(modelName, field).getFieldType())
        ).forEach(this.differFields::add);
        if (this.differFields.contains(ModelConstant.EFFECTIVE_START) && ModelManager.isTimelineModel(modelName)) {
            // When changing the `effectiveStart` field of the timeline model,
            // get the original `effectiveEnd` field at the same time for time comparison.
            this.differFields.add(ModelConstant.EFFECTIVE_END);
        }
    }

    /**
     * Combine the dependent fields of cascaded fields and computed fields with the request data,
     * by overwriting the original data with the request data.
     *
     * @param newRows request data to be updated
     * @return merged result data
     */
    private List<Map<String, Object>> mergeToOriginalData(Collection<Map<String, Object>> newRows, Map<Serializable, Map<String, Object>> originalRowsMap) {
        // TODO: Skip when there no affected cascaded fields and computed fields.
        List<Map<String, Object>> mergedRows = new ArrayList<>();
        newRows.forEach(newRow -> {
            Serializable pKey = (Serializable) newRow.get(primaryKey);
            // Ignore the update data when the id does not exist
            if (originalRowsMap.containsKey(pKey)) {
                Map<String, Object> mergedRow = new HashMap<>();
                mergedRow.putAll(originalRowsMap.get(pKey));
                mergedRow.putAll(newRow);
                mergedRows.add(mergedRow);
            }
        });
        return mergedRows;
    }

    /**
     * Compare between the new and old data rows, only retain the primary key and modified field values.
     * The `version` field is special:
     * if the version is different, an exception is thrown, and if the version is the same, the version is +1.
     *
     * @param modelName model name
     * @param primaryKey primary key field name
     * @param differentFields fields to be compared for differences
     * @param newRows new data rows
     * @param originalRowsMap original database values, key is the primary key, value is the original data row.
     * @return difference data rows
     */
    public static List<Map<String, Object>> differFromPrevious(String modelName, String primaryKey, Set<String> differentFields, List<Map<String, Object>> newRows, Map<Serializable, Map<String, Object>> originalRowsMap) {
        List<Map<String, Object>> differRows = new ArrayList<>();
        boolean controlVersion = ModelManager.isVersionControl(modelName);
        newRows.forEach(newRow -> {
            Serializable pKey = (Serializable) newRow.get(primaryKey);
            if (originalRowsMap.containsKey(pKey)) {
                Map<String, Object> originalRow = new HashMap<>();
                Map<String, Object> differRow = new HashMap<>();
                differentFields.forEach(field -> {
                    Object newValue = newRow.get(field);
                    Object oldValue = originalRowsMap.get(pKey).get(field);
                    boolean isValueModified = valueModified(modelName, field, newValue, oldValue);
                    if (controlVersion && ModelConstant.VERSION.equals(field)) {
                        // Version control, if the version is different, throw a VersionException exception,
                        if (isValueModified) {
                            throw new VersionException("""
                                    Data version does not match, may have been modified, please refresh and try again!
                                    Provided value: {0}, database value: {1}, the two are not equal.""",
                                    newValue, oldValue);
                        } else {
                            // Increment version by 1. But when constructing the where condition, it will be -1.
                            Integer newVersion =  (Integer) oldValue + 1;
                            differRow.put(field, newVersion);
                            originalRow.put(field, oldValue);
                        }
                    } else if (isValueModified) {
                        differRow.put(field, newRow.get(field));
                        // Retain the original value of the modified field
                        originalRow.put(field, originalRowsMap.get(pKey).get(field));
                    }
                });
                // When the effectiveStart is modified, the effectiveEnd data is retained for subsequent time conditions
                if (differentFields.contains(ModelConstant.EFFECTIVE_START)) {
                    originalRow.put(ModelConstant.EFFECTIVE_END, originalRowsMap.get(pKey).get(ModelConstant.EFFECTIVE_END));
                }
                if (!differRow.isEmpty()) {
                    // Keep the `ID` business key field for timeline model rows, to query other affected slices.
                    differRow.put(ModelConstant.ID, newRow.get(ModelConstant.ID));
                    differRow.put(primaryKey, pKey);
                    differRows.add(differRow);
                    originalRowsMap.put(pKey, originalRow);
                }
            }
        });
        return differRows;
    }

    /**
     * Determine whether the field value has been modified, by judging whether the new and old values are equal.
     *
     * @param modelName model name
     * @param fieldName field name
     * @param newValue new value of the field, for Date and DateTime type fields, the field value needs to be
     *                converted to String for comparison, to avoid unequal equals due to different object class.
     * @param oldValue old value of the field, for Date and DateTime type fields,
     *                 the original value read from database is already String
     * @return whether the field value has been modified
     */
    private static boolean valueModified(String modelName, String fieldName, Object newValue, Object oldValue) {
        if (newValue instanceof LocalDate) {
            newValue = TimeConstant.DATE_FORMATTER.format((LocalDate) newValue);
        } else if (newValue instanceof LocalDateTime) {
            newValue = TimeConstant.DATETIME_FORMATTER.format((LocalDateTime) newValue);
        } else if (newValue instanceof Date) {
            FieldType fieldType = ModelManager.getModelField(modelName, fieldName).getFieldType();
            if (fieldType.equals(FieldType.DATE)) {
                newValue = TimeConstant.DATE_FORMATTER.format(DateUtils.dateToLocalDate(newValue));
            } else if (fieldType.equals(FieldType.DATE_TIME)) {
                newValue = TimeConstant.DATETIME_FORMATTER.format(DateUtils.dateToLocalDateTime(newValue));
            }
            // After converting to a String, if they are equal, it is judged as not modified.
            return !(newValue.equals(oldValue));
        }
        return !Objects.equals(newValue, oldValue);
    }
}
