package info.openmeta.framework.orm.meta;

import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;
import info.openmeta.framework.base.config.TenantConfig;
import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.enums.IdStrategy;
import info.openmeta.framework.orm.jdbc.JdbcService;
import info.openmeta.framework.orm.utils.ListUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Model Manager, maintaining model metadata and field metadata in memory.
 */
@Component
@Slf4j
public class ModelManager {

    private static final Map<String, MetaModel> MODEL_MAP = new ConcurrentHashMap<>(200);
    private static final Map<String, Map<String, MetaField>> MODEL_FIELDS = new ConcurrentHashMap<>(200);

    @Autowired
    private JdbcService<?> jdbcService;

    /**
     * Initialize ModelManager, load MetaModel and MetaField data from the database
     */
    public void init() {
        MODEL_MAP.clear();
        MODEL_FIELDS.clear();
        // Set the initialization Context
        ContextHolder.setContext(new Context());
        List<MetaModel> models = jdbcService.selectMetaEntityList("SysModel", MetaModel.class, null);
        List<MetaField> fields = jdbcService.selectMetaEntityList("SysField", MetaField.class, null);
        if (ListUtils.allNotNull(models, fields)) {
            this.initModels(models);
            // Initialize basic field information
            this.initBasicFields(fields);
            // Validate model attributes
            this.validateModelAttributes(models);
            // Validate field attributes
            this.validateFieldAttributes(fields);
        }
    }

    /**
     * Initialize model data: MODEL_MAP, MODEL_FIELDS
     * @param models model metadata
     */
    private void initModels(List<MetaModel> models) {
        models.forEach(model -> {
            MODEL_MAP.put(model.getModelName(), model);
            MODEL_FIELDS.put(model.getModelName(), new HashMap<>(4));
        });
    }

    /**
     * Initialize basic field information: MODEL_FIELDS
     * @param fields field metadata
     */
    private void initBasicFields(List<MetaField> fields) {
        fields.forEach(field -> {
            Assert.notNull(field.getFieldType(), "The fieldType of field metadata is not supported: {0}", field);
            // Convert the string default value to the default value object
            field.setDefaultValueObject(FieldType.convertStringToObject(field.getFieldType(), field.getDefaultValue()));
            if ((FieldType.TO_ONE_TYPES.contains(field.getFieldType()))) {
                // For OneToOne/ManyToOne fields, the `relatedField` is default to id.
                field.setRelatedField(ModelConstant.ID);
            }
            if (field.isComputed()) {
                Assert.notBlank(field.getExpression(), "The formula of computed field {0}:{1} cannot be empty!",
                        field.getModelName(), field.getFieldName());
                try {
                    // Extract and set `dependentFields` for computed fields
                    List<String> dependentFields = ComputeUtils.compile(field.getExpression()).getVariableFullNames();
                    field.setDependentFields(dependentFields);
                } catch (ExpressionSyntaxErrorException e) {
                    throw new IllegalArgumentException("Computed field {0}:{1}, formula syntax error: {2}\n{3}",
                            field.getModelName(), field.getFieldName(), field.getExpression(), e.getMessage());
                }
            }
            Assert.isTrue(MODEL_FIELDS.containsKey(field.getModelName()),
                    "Model for field does not exist in model metadata: {0}", field);
            MODEL_FIELDS.get(field.getModelName()).put(field.getFieldName(), field);
        });
    }

    /**
     * Validate the model attributes after loading basic fields info.
     *
     * @param metaModels model metadata list
     */
    public void validateModelAttributes(List<MetaModel> metaModels){
        for (MetaModel metaModel : metaModels) {
            // Check if the model name is valid.
            Assert.isTrue(StringTools.isModelName(metaModel.getModelName()),
                    "Model name `{0}` does not meet the specification!", metaModel.getModelName());
            // TODO: Assert validation after changing the `tableName` to computed field.
            metaModel.setTableName(StringTools.toUnderscoreCase(metaModel.getModelName()));
            // Assert.isTrue(StringTools.isSnakeNameValid(metaModel.getTableName()),
            // "The table name for model `{0}` does not meet the specification!", metaModel.getModelName(), metaModel.getTableName());

            // Check if the soft delete model has a `disabled` field
            validateSoftDeleted(metaModel);
            // Check and complete the model-level displayName configuration.
            validateModelDisplayName(metaModel);
            // Check and complete the searchName configuration
            validateSearchName(metaModel);
            // Check if the timeline model contains the required timeline fields: sliceId, effectiveStartDate, effectiveEndDate
            validateTimelineFields(metaModel.getModelName());
            // Check if the multi-tenant model contains the tenantId field
            validateMultiTenant(metaModel);
            // Check if the optimistic lock control model contains the version field
            validateVersionField(metaModel);
            // Check if the model dataSource attribute is valid
            validateModelDataSource(metaModel);
            // Check if the model businessKey attribute is valid
            validateBusinessKey(metaModel);
        }
    }

    /**
     * Validate the field attributes after loading basic fields info.
     *
     * @param metaFields field metadata list
     */
    public void validateFieldAttributes(List<MetaField> metaFields){
        for (MetaField metaField : metaFields) {
            Assert.isTrue(StringTools.isFieldName(metaField.getFieldName()),
                    "{0}:{1}, the fieldName is invalid!", metaField.getModelName(), metaField.getFieldName());
            // TODO: remove `set` after changing the `columnName` to computed field.
            metaField.setColumnName(StringTools.toUnderscoreCase(metaField.getFieldName()));
            // Assert.isTrue(StringTools.isSnakeNameValid(metaField.getColumnName()), "{0}:{1}, the columnName {2} is invalid!", metaField.getModelName(), metaField.getFieldName(), metaField.getColumnName());
            Assert.notTrue(ModelConstant.VIRTUAL_FIELDS.contains(metaField.getFieldName()),
                    "Model field {0}:{1} cannot use a virtual field name!", metaField.getModelName(), metaField.getFieldName());
            // Check if the related field is valid
            if (FieldType.RELATED_TYPES.contains(metaField.getFieldType())) {
                validateRelatedField(metaField);
            }
            // Check if the cascaded field is valid
            if (StringUtils.isNotBlank(metaField.getCascadedField())) {
                validateCascadedField(metaField);
            }
            // Check if the computed field is valid
            if (metaField.isComputed()) {
                validateComputedField(metaField);
            }
            // Verify and update the `readonly` attribute of field
            verifyReadonlyAttribute(metaField);
            // Verify and update the `dynamic` attribute of field
            verifyDynamicAttribute(metaField);
        }
    }

    /**
     * Check if the soft delete model has a `disabled` field.
     * When the data of soft deleted model is deleted, set `disabled=true`.
     * The active data can be queried by filter `disabled=false`.
     *
     * @param metaModel model metadata object
     */
    private static void validateSoftDeleted(MetaModel metaModel) {
        if (metaModel.isSoftDelete()) {
            Assert.isTrue(existField(metaModel.getModelName(), ModelConstant.SOFT_DELETED_FIELD),
                    "Model {0} `softDelete = true`, but field `disabled` does not exist!", metaModel.getLabelName());
        }
    }

    /**
     * Validate the model-level `displayName`, the display name fields are ordered as defined.
     * When the `displayName` is not defined, if there is a `name` field, it will be displayed as `displayName`.
     * If there is no `name` field, it will be displayed as `id`.
     *
     * @param metaModel model metadata object
     */
    private static void validateModelDisplayName(MetaModel metaModel) {
        List<String> displayName = metaModel.getDisplayName();
        if (!CollectionUtils.isEmpty(displayName)) {
            Assert.isTrue(!displayName.contains(ModelConstant.DISPLAY_NAME),
                    "Model {0} displayName cannot contain the 'displayName' keyword itself!", metaModel.getModelName());
            validateStoredFields(metaModel.getModelName(), displayName);
        } else if (existField(metaModel.getModelName(), "name")) {
            metaModel.setDisplayName(Collections.singletonList("name"));
        } else {
            metaModel.setDisplayName(Collections.singletonList(ModelConstant.ID));
        }
    }

    /**
     * Validate the field-level displayName, multiple display name fields are ordered.
     *
     * @param metaField field metadata object
     */
    private static void validateFieldDisplayName(MetaField metaField) {
        List<String> displayName = metaField.getDisplayName();
        if (!CollectionUtils.isEmpty(displayName)) {
            Assert.isTrue(!displayName.contains(ModelConstant.DISPLAY_NAME),
                    "The displayName if field {0} cannot contain the 'displayName' keyword itself!", metaField.getFieldName());
            validateStoredFields(metaField.getRelatedModel(), displayName);
        }
    }

    /**
     * Validate the searchName.
     * When the `searchName` is not defined, if there is a `name` field, it will be used as `searchName`.
     *
     * @param metaModel model metadata object
     */
    private static void validateSearchName(MetaModel metaModel) {
        List<String> searchName = metaModel.getSearchName();
        if (!CollectionUtils.isEmpty(searchName)) {
            Assert.isTrue(!searchName.contains(ModelConstant.SEARCH_NAME),
                    "The searchName of model {0} cannot contain the 'searchName' keyword itself!", metaModel.getModelName());
            searchName.forEach(field -> {
                FieldType fieldType = getModelField(metaModel.getModelName(), field).getFieldType();
                Assert.isTrue(FieldType.STRING.equals(fieldType),
                        "The `searchName` attribute only supports string type fields, not model {0} field {1} type {2}!",
                        metaModel.getModelName(), field, fieldType.getType());
            });
        } else if (existField(metaModel.getModelName(), "name")) {
            metaModel.setSearchName(Collections.singletonList("name"));
        }
    }

    /**
     * Validate whether the timeline model contains the required timeline fields:
     *      sliceId, id, effectiveStartDate, effectiveEndDate.
     *
     * @param modelName model name
     */
    private static void validateTimelineFields(String modelName) {
        if (isTimelineModel(modelName)) {
            Set<String> modelFields = MODEL_FIELDS.get(modelName).keySet();
            Set<String> subFields = new HashSet<>(ModelConstant.TIMELINE_FIELDS);
            subFields.removeAll(modelFields);
            Assert.isTrue(subFields.isEmpty(), "Timeline model {0} must contain the required fields {1}!", modelName, subFields);
        }
    }

    /**
     * Validate whether the multi-tenant model contains the `tenantId` field.
     *
     * @param metaModel model metadata object
     */
    private static void validateMultiTenant(MetaModel metaModel) {
        if (metaModel.isMultiTenant()) {
            Assert.isTrue(MODEL_FIELDS.get(metaModel.getModelName()).containsKey(ModelConstant.TENANT_ID),
                    "The multi-tenant model {0} must contain the `tenantId` field!", metaModel.getModelName());
        }
    }

    /**
     * Validate whether the optimistic lock control model contains the `version` field.
     *
     * @param metaModel model metadata object
     */
    private static void validateVersionField(MetaModel metaModel) {
        if (metaModel.isVersionLock()) {
            Assert.isTrue(MODEL_FIELDS.get(metaModel.getModelName()).containsKey(ModelConstant.VERSION),
                    "The model {0} must contain the `version` field when using optimistic lock control!",
                    metaModel.getModelName());
        }
    }

    /**
     * Validate the model dataSource attribute.
     * The system model cannot be configured with a dataSource.
     *
     * @param metaModel model metadata object
     */
    private static void validateModelDataSource(MetaModel metaModel) {
        String dataSource = metaModel.getDataSource();
        if (ModelConstant.SYSTEM_MODEL.contains(metaModel.getModelName()) && StringUtils.isNotBlank(dataSource)) {
            throw new IllegalArgumentException("The system model {0} cannot be configured with a dataSource {1}!",
                    metaModel.getModelName(), dataSource);
        }
    }

    /**
     * Check if the fields exists in the model
     * @param metaModel model metadata object
     */
    private static void validateBusinessKey(MetaModel metaModel) {
        List<String> businessKey = metaModel.getBusinessKey();
        if (CollectionUtils.isEmpty(businessKey)) {
            return;
        }
        validateStoredFields(metaModel.getModelName(), businessKey);
        Set<String> businessKeySet = new HashSet<>(businessKey);
        Assert.isTrue(businessKeySet.size() == businessKey.size(),
                "The businessKey of model {0} contains duplicate fields, {1}.",
                metaModel.getModelName(), businessKey);
    }

    /**
     * Check if the related field attributes are valid.
     *
     * @param metaField field metadata object
     */
    private static void validateRelatedField(MetaField metaField) {
        String relatedModel = metaField.getRelatedModel();
        Assert.isTrue(StringUtils.isNotBlank(relatedModel),
                "The relatedModel of the related field {0}:{1} cannot be empty!",
                metaField.getModelName(), metaField.getFieldName());
        Assert.isTrue(MODEL_MAP.containsKey(relatedModel),
                "The relatedModel {0} of the related field {1}:{2} does not exist in the model metadata!",
                relatedModel, metaField.getModelName(), metaField.getFieldName());
        validateFieldDisplayName(metaField);
    }

    /**
     * Check whether the `cascadedField` config is valid: `fieldA.fieldB`,
     * where `fieldA` is a ManyToOne/OneToOne field of the current model,
     * and `fieldB` is a stored field of the related model.
     * If current field is a stored field, the ManyToOne/OneToOne `fieldA` must be a stored field too.
     *
     * @param metaField field metadata object
     */
    private static void validateCascadedField(MetaField metaField) {
        String modelName = metaField.getModelName();
        String fieldName = metaField.getFieldName();
        Assert.isTrue(!ModelConstant.AUDIT_FIELDS.contains(fieldName),
                "The field {0} of model {1} is an audit field and cannot be defined as a cascaded field!",
                fieldName, modelName);
        String[] cascadedFields = StringUtils.split(metaField.getCascadedField(), ".");
        Assert.isTrue(cascadedFields.length == 2,
                "The `cascadedField` {0} of model {1} field {2} does not valid! Only `fieldA.fieldB` format is allowed.",
                metaField.getCascadedField(), modelName, fieldName);
        MetaField leftField = getModelField(modelName, cascadedFields[0]);
        Set<String> modelAllFields = MODEL_FIELDS.get(modelName).keySet();
        Assert.isTrue(modelAllFields.contains(cascadedFields[0]) && FieldType.TO_ONE_TYPES.contains(leftField.getFieldType()),
                "The `cascadedField` {0} of model {1} field {2} does not valid! The field `{3}` is not a ManyToOne/OneToOne field of current model.",
                metaField.getCascadedField(), modelName, fieldName, cascadedFields[0]);
        Assert.isTrue(isStored(leftField.getRelatedModel(), cascadedFields[1]),
                "The `cascadedField` {0} of model {1} field {2} does not valid! The field `{3}` is a dynamic field of related model `{4}`.",
                metaField.getCascadedField(), modelName, fieldName, cascadedFields[1], leftField.getRelatedModel());
    }

    /**
     * Check whether the dependent fields of the computed field belong to the same model.
     * Computed fields only used in single model calculations.
     * If the computed field is stored, it must depend on stored fields.
     *
     * @param metaField field metadata object
     */

    private static void validateComputedField(MetaField metaField) {
        Assert.isTrue(!ModelConstant.AUDIT_FIELDS.contains(metaField.getFieldName()),
                "The field {0} of model {1} is an audit field and cannot be defined as a computed field!",
                metaField.getFieldName(), metaField.getModelName());
        validateModelFields(metaField.getModelName(), metaField.getDependentFields());
        if (!metaField.isDynamic()) {
            // Stored computed field, must depend on stored fields.
            validateStoredFields(metaField.getModelName(), metaField.getDependentFields());
        }
    }

    /**
     * Check if the fields of the model are stored fields
     *
     * @param modelName model name
     * @param fields field name list
     */
    public static void validateStoredFields(String modelName, List<String> fields) {
        Set<String> dynamicFields = fields.stream().filter(f -> getModelField(modelName, f).isDynamic())
                .collect(Collectors.toSet());
        Assert.isTrue(CollectionUtils.isEmpty(dynamicFields),
                "Not all fields {1} of model {0} are stored fields!", dynamicFields, modelName);
    }

    /**
     * Verify and update the `readonly` attribute of the field.
     * The `readonly` attribute is automatically set for the following fields:
     *     1. Fields in the `AUDIT_FIELDS` list, including audit fields, `version`, `sliceId`, `tenantId`.
     *     2. TenantId field if enable multi-tenant.
     *     3. Version field of the optimistic lock control model.
     *     4. SliceId field of the timeline model.
     *     5. Computed fields, cascaded fields.
     *     6. `id` field of the model, except for EXTERNAL_ID strategy.
     *
     * @param metaField field metadata object
     */
    private static void verifyReadonlyAttribute(MetaField metaField) {
        String model = metaField.getModelName();
        if (ModelConstant.AUDIT_FIELDS.contains(metaField.getFieldName())) {
            metaField.setReadonly(true);
        } else if (TenantConfig.isEnableMultiTenancy() && ModelConstant.TENANT_ID.equals(metaField.getFieldName())) {
            metaField.setReadonly(true);
        } else if (MODEL_MAP.get(model).isVersionLock() && ModelConstant.VERSION.equals(metaField.getFieldName())) {
            metaField.setReadonly(true);
        } else if (MODEL_MAP.get(model).isTimeline() && ModelConstant.SLICE_ID.equals(metaField.getFieldName())) {
            metaField.setReadonly(true);
        } else if (metaField.isComputed() || StringUtils.isNotBlank(metaField.getCascadedField())) {
            metaField.setReadonly(true);
        } else if (ModelConstant.ID.equals(metaField.getFieldName())
                && !IdStrategy.EXTERNAL_ID.equals(MODEL_MAP.get(model).getIdStrategy())) {
            metaField.setReadonly(true);
        }
    }

    /**
     * Verify and update the `dynamic` attribute of the field.
     * If the field is a OneToMany/ManyToMany field, set `dynamic = true`.
     *
     * @param metaField field metadata object
     */
    private static void verifyDynamicAttribute(MetaField metaField) {
        if (FieldType.TO_MANY_TYPES.contains(metaField.getFieldType())) {
            metaField.setDynamic(true);
        }
    }

    /**
     * Check if the model exists
     *
     * @param modelName model name
     * @return true or false
     */
    public static boolean existModel(String modelName){
        return MODEL_MAP.containsKey(modelName);
    }

    /**
     * Check if the model exists, if not, throw an exception
     * @param modelName model name
     */
    public static void validateModel(String modelName){
        Assert.isTrue(existModel(modelName), "Model {0} does not exist in the model metadata!", modelName);
    }

    /**
     * Check if the specified field exists, if not, throw an exception
     * @param modelName model name
     * @param fieldName field name
     */
    public static void validateModelField(String modelName, String fieldName){
        validateModel(modelName);
        Assert.isTrue(MODEL_FIELDS.get(modelName).containsKey(fieldName),
                "Model {0} does not exist field {1}!", modelName, fieldName);
    }

    /**
     * Check if all the specified fields exist in the model, if not, throw an exception.
     *
     * @param modelName model name
     * @param fields field name list
     */
    public static void validateModelFields(String modelName, Collection<String> fields){
        validateModel(modelName);
        if (CollectionUtils.isEmpty(fields)) {
            return;
        }
        Set<String> accessFields = new HashSet<>(fields);
        accessFields.removeAll(MODEL_FIELDS.get(modelName).keySet());
        Assert.isTrue(accessFields.isEmpty(), "Model {0} does not exist fields {1}!", modelName, accessFields);
    }

    /**
     * Get the model metadata object by model name
     *
     * @param modelName model name
     * @return model metadata object
     */
    public static MetaModel getModel(String modelName){
        validateModel(modelName);
        return MODEL_MAP.get(modelName);
    }

    /**
     * Get the model primary key field name, the physical primary key field of the timeline model is `sliceId`,
     * and the primary key field of other models is `id`.
     *
     * @param modelName model name
     * @return primary key field name
     */
    public static String getModelPrimaryKey(String modelName) {
        return isTimelineModel(modelName) ? ModelConstant.SLICE_ID : ModelConstant.ID;
    }

    /**
     * Get the model primary key field object, the physical primary key of the timeline model is `sliceId`,
     * and the primary key of other models is `id`.
     *
     * @param modelName model name
     * @return primary key field object
     */
    public static MetaField getModelPrimaryKeyField(String modelName) {
        return MODEL_FIELDS.get(modelName).get(isTimelineModel(modelName) ? ModelConstant.SLICE_ID : ModelConstant.ID);
    }

    /**
     * Get the MetaField object collection of the model.
     *
     * @param modelName model name
     * @return MetaField object collection
     */
    public static List<MetaField> getModelFields(String modelName){
        validateModel(modelName);
        return List.copyOf(MODEL_FIELDS.get(modelName).values());
    }

    /**
     * Get the copyable fields of the specified model.
     *
     * @param modelName model name
     * @return copyable fields
     */
    public static List<String> getModelCopyableFields(String modelName) {
        return MODEL_FIELDS.get(modelName).keySet().stream()
                .filter(fieldName -> {
                    if (ModelConstant.AUDIT_FIELDS.contains(fieldName)) {
                        return false;
                    } else if (isTimelineModel(modelName)) {
                        if (ModelConstant.ID.equals(fieldName)) {
                            return true;
                        } else return !ModelConstant.SLICE_ID.equals(fieldName);
                    } else return !ModelConstant.ID.equals(fieldName) && !ModelConstant.EXTERNAL_ID.equals(fieldName);
                })
                .toList();
    }

    /**
     * Get the MetaField object by model name and field name.
     *
     * @param modelName model name
     * @param fieldName field name
     * @return MetaField object
     */
    public static MetaField getModelField(String modelName, String fieldName){
        validateModelField(modelName, fieldName);
        return MODEL_FIELDS.get(modelName).get(fieldName);
    }

    /**
     * Get the column name by model name and field name.
     *
     * @param modelName model name
     * @param fieldName field name
     * @return Column column name
     */
    public static String getModelFieldColumn(String modelName, String fieldName){
        validateModelField(modelName, fieldName);
        return MODEL_FIELDS.get(modelName).get(fieldName).getColumnName();
    }

    /**
     * Get the translation modelName of the specified data model.
     * The translation model name equals to `modelName + "Trans"`, which stores the translations of the data model.
     *
     * @param modelName data model name
     * @return the translation modelName of the data model
     */
    public static String getTranslationModelName(String modelName) {
        validateModel(modelName);
        return modelName + ModelConstant.MODEL_TRANS_SUFFIX;
    }

    /**
     * Get the translation table name of the data model.
     * The translation table name equals to `table_name + "_trans"`, which stores the translations of the data model.
     *
     * @param modelName data model name
     * @return the translation table name of the data model
     */
    public static String getTranslationTableName(String modelName) {
        String transModel = getTranslationModelName(modelName);
        return StringTools.toUnderscoreCase(transModel);
    }

    /**
     * Get the cascaded fields of the model.
     *
     * @param modelName model name
     * @param dynamic dynamic attribute
     * @return cascaded fields
     */
    public static List<MetaField> getModelCascadedFields(String modelName, Boolean dynamic){
        return MODEL_FIELDS.get(modelName).values().stream()
                .filter(metaField -> StringUtils.isNotBlank(metaField.getCascadedField())
                        && Objects.equals(metaField.isDynamic(), dynamic))
                .collect(Collectors.toList());
    }

    /**
     * Get the computed fields of the model.
     *
     * @param modelName model name
     * @param dynamic dynamic attribute
     * @return computed fields
     */
    public static List<MetaField> getModelComputedFields(String modelName, Boolean dynamic){
        return MODEL_FIELDS.get(modelName).values().stream()
                .filter(metaField -> metaField.isComputed() && Objects.equals(metaField.isDynamic(), dynamic))
                .collect(Collectors.toList());
    }

    /**
     * Get the encrypted fields of the model.
     * Currently only support String fields for encryption.
     *
     * @param modelName model name
     * @return encrypted fields
     */
    public static Set<String> getModelEncryptedFields(String modelName){
        return MODEL_FIELDS.get(modelName).values().stream()
                .filter(metaField -> metaField.isEncrypted() && metaField.getFieldType().equals(FieldType.STRING))
                .map(MetaField::getFieldName).collect(Collectors.toSet());
    }

    /**
     * Get the masking fields of the model.
     * Currently, only String fields are supported for masking.
     *
     * @param modelName model name
     * @return masking fields
     */
    public static Set<MetaField> getModelMaskingFields(String modelName){
        return MODEL_FIELDS.get(modelName).values().stream()
                .filter(metaField -> metaField.getMaskingType() != null
                        && metaField.getFieldType().equals(FieldType.STRING))
                .collect(Collectors.toSet());
    }

    /**
     * Get the fields of the model that are read by default, including `autoBindMany = true` OneToMany/ManyToMany fields.
     * To read OneToMany/ManyToMany fields which `autoBindMany = false`, the client needs to use QueryParams/SubQuery.
     *
     * @param modelName model name
     * @return fields collection
     */
    public static Set<String> getModelDefaultReadFields(String modelName){
        validateModel(modelName);
        return MODEL_FIELDS.get(modelName).values().stream()
                .filter(metaField -> !FieldType.TO_MANY_TYPES.contains(metaField.getFieldType()) || metaField.isAutoBindMany())
                .map(MetaField::getFieldName).collect(Collectors.toSet());
    }

    /**
     * Get the fields of the model, without OneToMany/ManyToMany fields.
     *
     * @param modelName model name
     * @return normal fields collection
     */
    public static Set<String> getModelFieldsWithoutXToMany(String modelName){
        validateModel(modelName);
        return MODEL_FIELDS.get(modelName).values().stream()
                .filter(metaField -> !FieldType.TO_MANY_TYPES.contains(metaField.getFieldType()))
                .map(MetaField::getFieldName).collect(Collectors.toSet());
    }

    /**
     * Get the updatable stored fields of the single model, without OneToMany/ManyToMany and `readonly=true` fields.
     *
     * @param modelName model name
     * @return stored field collection
     */
    public static Set<String> getModelUpdatableFieldsWithoutXToMany(String modelName){
        validateModel(modelName);
        return getModelFieldsWithoutXToMany(modelName).stream()
                .filter(field -> !getModelField(modelName, field).isReadonly())
                .collect(Collectors.toSet());
    }

    /**
     * Get the updatable fields of the model, which can be directly assigned, including the OneToMany/ManyToMany fields.
     * Excluding the fields of `readonly = true`.
     *
     * @param modelName model name
     * @return updatable field set
     */
    public static Set<String> getModelUpdatableFields(String modelName){
        validateModel(modelName);
        return MODEL_FIELDS.get(modelName).values().stream()
                .filter(metaField -> !metaField.isReadonly())
                .map(MetaField::getFieldName).collect(Collectors.toSet());
    }

    /**
     * Get the stored fields of the model.
     * Excluding OneToMany/ManyToMany fields, `dynamic` cascaded fields and `dynamic` computed fields.
     *
     * @param modelName model name
     * @return stored field list
     */
    public static List<String> getModelStoredFields(String modelName){
        validateModel(modelName);
        return MODEL_FIELDS.get(modelName).values().stream()
                .filter(metaField -> !metaField.isDynamic())
                .map(MetaField::getFieldName).collect(Collectors.toList());
    }

    /**
     * Get the fields of the specified fieldType in the model.
     *
     * @param modelName model name
     * @param fieldType field data type
     * @return field object set
     */
    public static Set<MetaField> getModelFieldsWithType(String modelName, FieldType fieldType){
        validateModel(modelName);
        return MODEL_FIELDS.get(modelName).values().stream()
                .filter(field -> fieldType.equals(field.getFieldType()))
                .collect(Collectors.toSet());
    }

    /**
     * Get the displayed fields of the related model by the relational field.
     * If the field-level `displayName` is not defined, take the `displayName` of the related model.
     *
     * @param metaField relational field object
     * @return related model field list
     */
    public static List<String> getFieldDisplayName(MetaField metaField){
        return CollectionUtils.isEmpty(metaField.getDisplayName()) ?
                MODEL_MAP.get(metaField.getRelatedModel()).getDisplayName() : metaField.getDisplayName();
    }

    /**
     * Get the numeric fields of the model, including stored and dynamic fields.
     *
     * @param modelName model name
     * @return numeric field set
     */
    public static Set<String> getModelNumericFields(String modelName){
        validateModel(modelName);
        return MODEL_FIELDS.get(modelName).values().stream()
                .filter(f -> FieldType.NUMERIC_TYPES.contains(f.getFieldType())
                        && !ModelConstant.RESERVED_KEYWORD.contains(f.getFieldName()))
                .map(MetaField::getFieldName).collect(Collectors.toSet());
    }

    /**
     * Get the stored numeric fields of the model.
     *
     * @param modelName model name
     * @return stored numeric field set
     */
    public static Set<String> getModelStoredNumericFields(String modelName){
        validateModel(modelName);
        return MODEL_FIELDS.get(modelName).values().stream()
                .filter(f -> FieldType.NUMERIC_TYPES.contains(f.getFieldType())
                        && !f.isDynamic()
                        && !ModelConstant.ID.equals(f.getFieldName())
                        && !ModelConstant.AUDIT_FIELDS.contains(f.getFieldName()))
                .map(MetaField::getFieldName).collect(Collectors.toSet());
    }

    /**
     * Get the last field object of the custom cascaded field.
     * Take `field1.field2.field3` as an example, get the `field3` MetaField object.
     *
     * @param modelName the main model name
     * @param fullFieldName custom cascaded field name like `field1.field2.field3`
     * @return last field object
     */
    public static MetaField getLastFieldOfCascaded(String modelName, String fullFieldName) {
        String[] fieldsArray = StringUtils.split(fullFieldName, ".");
        Assert.isTrue(fieldsArray.length - 1 <  BaseConstant.CASCADE_LEVEL,
                "Custom cascaded field {0} cannot exceed the max cascaded levels of {1}!",
                fullFieldName, BaseConstant.CASCADE_LEVEL);
        MetaField metaField = null;
        for (int i = 0; i < fieldsArray.length; i ++) {
            metaField = getModelField(modelName, fieldsArray[i]);
            if (i < fieldsArray.length - 1) {
                Assert.isTrue(FieldType.TO_ONE_TYPES.contains(metaField.getFieldType()),
                        "The field {0} in custom cascaded field {1} must be ManyToOne/OneToOne field!",
                        metaField.getFieldName(), fullFieldName);
            } else {
                Assert.notTrue(metaField.isDynamic(),
                        "The last field {0} in custom cascaded field {1} must be a stored field in model {2}!",
                        metaField.getFieldName(), fullFieldName, metaField.getModelName());
            }
            modelName = metaField.getRelatedModel();
        }
        return metaField;
    }

    /**
     * Check if the model has the specified field.
     *
     * @param modelName model name
     * @param fieldName field name
     * @return true or false
     */
    public static boolean existField(String modelName, String fieldName){
        validateModel(modelName);
        return MODEL_FIELDS.get(modelName).containsKey(fieldName);
    }

    /**
     * Determine whether the model field is stored in the database. `dynamic = true` is not stored in the database,
     * including OneToMany/ManyToMany fields, dynamic cascaded field and dynamic computed field.
     *
     * @param modelName model name
     * @param fieldName field name
     * @return true or false
     */
    public static boolean isStored(String modelName, String fieldName){
        validateModelField(modelName, fieldName);
        MetaField metaField = MODEL_FIELDS.get(modelName).get(fieldName);
        return !metaField.isDynamic();
    }

    /**
     * Determine whether the model is a timeline model.
     *
     * @param modelName model name
     * @return true or false
     */
    public static boolean isTimelineModel(String modelName) {
        validateModel(modelName);
        return MODEL_MAP.get(modelName).isTimeline();
    }

    /**
     * Determine whether the model is a soft delete model.
     *
     * @param modelName model name
     * @return true or false
     */
    public static boolean isSoftDeleted(String modelName) {
        return ModelManager.getModel(modelName).isSoftDelete();
    }

    /**
     * Determine whether the model has enabled version control.
     *
     * @param modelName model name
     * @return true or false
     */
    public static boolean isVersionControl(String modelName) {
        return ModelManager.getModel(modelName).isVersionLock();
    }

    /**
     * Determine whether the model data needs to be isolated by tenantId.
     *
     * @param modelName model name
     * @return true or false
     */
    public static boolean isMultiTenant(String modelName) {
        return TenantConfig.isEnableMultiTenancy() && ModelManager.getModel(modelName).isMultiTenant();
    }

    /**
     * Get the ID strategy config of the model by model name.
     * If not configured, the default is DB_AUTO_ID.
     *
     * @param modelName model name
     * @return IdStrategy
     */
    public static IdStrategy getIdStrategy(String modelName) {
        IdStrategy idStrategy = ModelManager.getModel(modelName).getIdStrategy();
        return idStrategy == null ? IdStrategy.DB_AUTO_ID : idStrategy;
    }

}
