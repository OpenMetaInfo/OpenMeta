package info.openmeta.starter.designer.version.impl;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.utils.BeanTool;
import info.openmeta.starter.designer.ddl.DDLFactory;
import info.openmeta.starter.designer.dto.ModelChangesDTO;
import info.openmeta.starter.designer.dto.RowChangeDTO;
import info.openmeta.starter.designer.entity.DesignField;
import info.openmeta.starter.designer.entity.DesignModel;
import info.openmeta.starter.designer.entity.DesignModelIndex;
import info.openmeta.starter.designer.version.VersionDdl;
import jakarta.validation.constraints.NotNull;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.*;

/**
 * VersionDdl implementation
 */
@Component
public class VersionDdlImpl implements VersionDdl {

    /** Properties related to database columns in field metadata, generate DDL statements when these properties change **/
    private static final Set<String> COLUMN_PROPERTIES = Set.of("name", "length", "scale");
    private static final Set<String> INDEX_PROPERTIES = Set.of("indexName", "indexFields", "uniqueIndex");

    /**
     * Analyze and construct table structure DDL statements based on model and field change records
     *
     * @param modelChanges Model change records, including add, modify, delete records
     * @param fieldChanges Field change records, including add, modify, delete records
     * @return DDL SQL related to table structure
     */
    @Override
    public String generateTableDDL(ModelChangesDTO modelChanges, ModelChangesDTO fieldChanges) {
        StringBuilder ddl = new StringBuilder();
        if (modelChanges == null && fieldChanges == null) {
            return ddl.toString();
        }
        // Filter
        Set<String> deletedModelNames = new HashSet<>();
        Map<String, DesignModel> createdModelMap = new HashMap<>();
        // Extract model deletions and new models
        if (modelChanges != null) {
            for (RowChangeDTO modelChange : modelChanges.getCreatedRows()) {
                DesignModel designModel = BeanTool.mapToObject(modelChange.getCurrentData(), DesignModel.class);
                createdModelMap.put(designModel.getModelName(), designModel);
                // Set modelFields to an empty list
                designModel.setModelFields(new ArrayList<>());
            }
            for (RowChangeDTO modelChange : modelChanges.getDeletedRows()) {
                DesignModel designModel = BeanTool.mapToObject(modelChange.getCurrentData(), DesignModel.class);
                deletedModelNames.add(designModel.getModelName());
            }
        }
        if (!deletedModelNames.isEmpty()) {
            // Construct SQL statements based on deleted models
            ddl.append(buildDeleteTableSql(deletedModelNames));
        }
        // Construct SQL statements based on new models and their fields
        extractCreateModelFields(createdModelMap, fieldChanges);
        ddl.append(buildCreateTableSql(createdModelMap.values()));
        // Construct ALTER TABLE SQL statements based on updated fields, excluding already created or deleted models
        Set<String> excludedModelNames = Sets.union(createdModelMap.keySet(), deletedModelNames);
        ddl.append(buildAlterTableSql(fieldChanges, excludedModelNames));
        return ddl.toString();
    }

    /**
     * Get the list of fields for new models and assign them to the modelFields property of the model
     */
    private void extractCreateModelFields(Map<String, DesignModel> createdModelMap, ModelChangesDTO fieldChanges) {
        // Extract new storage field records and fill them into the modelFields property of DesignModel
        for (RowChangeDTO rowChangeDTO : fieldChanges.getCreatedRows()) {
            DesignField designField = BeanTool.mapToObject(rowChangeDTO.getCurrentData(), DesignField.class);
            if (createdModelMap.containsKey(designField.getModelName()) && !designField.getDynamic()) {
                DesignModel designModel = createdModelMap.get(designField.getModelName());
                designModel.getModelFields().add(designField);
            }
        }
    }

    /**
     * Construct DDL statements for deleting model tables
     * @param deletedModels Deleted model data
     * @return DDL statements for deleting tables
     */
    private StringBuilder buildDeleteTableSql(Collection<String> deletedModels) {
        StringBuilder ddl = new StringBuilder();
        if (!deletedModels.isEmpty()) {
            ddl.append("-- Delete tables:\n");
            deletedModels.forEach(model -> {
                String tableName = StringTools.toUnderscoreCase(model);
                ddl.append(DDLFactory.getInstance().dropTableDDL(tableName));
            });
        }
        return ddl;
    }

    /**
     * Construct DDL statements for creating new model tables
     * @param createdModels New model data
     * @return DDL statements for creating tables
     */
    private StringBuilder buildCreateTableSql(Collection<DesignModel> createdModels) {
        StringBuilder ddl = new StringBuilder();
        if (!createdModels.isEmpty()) {
            ddl.append("-- Create tables:\n");
            for (DesignModel designModel : createdModels) {
                String modelName = designModel.getModelName();
                Assert.isTrue(!CollectionUtils.isEmpty(designModel.getModelFields()),
                        "When publishing a new model {0}, its fields must be published simultaneously!", modelName);
                ddl.append(DDLFactory.getInstance().createTableDDL(designModel));
            }
        }
        return ddl;
    }

    /**
     * For scenarios where the model is not modified but only fields are added, modified, or deleted,
     * attach these fields to a new DesignModel object for alter table
     */
    private StringBuilder buildAlterTableSql(ModelChangesDTO fieldChanges, Set<String> excludedModelNames) {
        Map<String, DesignModel> updateModelMap = new HashMap<>();
        List<RowChangeDTO> updatedFields = fieldChanges.getUpdatedRows().stream()
                .filter(rowChangeDTO -> !Collections.disjoint(COLUMN_PROPERTIES, rowChangeDTO.getDataAfterChange().keySet()))
                .toList();
        Map<String, List<DesignField>> createdFieldsMap = extractModelFields(fieldChanges.getCreatedRows(), updateModelMap, excludedModelNames);
        Map<String, List<DesignField>> deletedFieldsMap = extractModelFields(fieldChanges.getDeletedRows(), updateModelMap, excludedModelNames);
        Map<String, List<DesignField>> updatedFieldsMap = extractModelFields(updatedFields, updateModelMap, excludedModelNames);

        StringBuilder ddl = new StringBuilder();
        if (!updateModelMap.isEmpty()) {
            ddl.append("-- Alter tables:\n");
            // Generate alter table DDL for storage fields
            updateModelMap.forEach((modelName, designModel) ->
                    ddl.append(DDLFactory.getInstance().alterTableDDL(designModel,
                            deletedFieldsMap.get(modelName),
                            createdFieldsMap.get(modelName),
                            updatedFieldsMap.get(modelName))));
        }
        return ddl;
    }

    /**
     * Extract change records for new, deleted, and updated storage fields
     */
    private Map<String, List<DesignField>> extractModelFields(List<RowChangeDTO> rowChangeDTOList,
                                                              Map<String, DesignModel> updateModelMap,
                                                              Set<String> excludedModelNames) {
        Map<String, List<DesignField>> modelFieldsMap = new HashMap<>();
        rowChangeDTOList.forEach(rowChangeDTO -> {
            DesignField designField = BeanTool.mapToObject(rowChangeDTO.getCurrentData(), DesignField.class);
            String modelName = designField.getModelName();
            if (excludedModelNames.contains(modelName) || designField.getDynamic()) {
                return;
            }
            updateModelMap.computeIfAbsent(modelName, key -> fillDesignModel(modelName));
            if (modelFieldsMap.containsKey(modelName)) {
                modelFieldsMap.get(modelName).add(designField);
            } else {
                modelFieldsMap.put(modelName, Lists.newArrayList(designField));
            }
        });
        return modelFieldsMap;
    }

    /**
     * Fill model object for alter table
     * @param modelName Model name
     * @return Model object
     */
    private DesignModel fillDesignModel(String modelName) {
        DesignModel designModel = new DesignModel();
        designModel.setModelName(modelName);
        designModel.setTableName(StringTools.toUnderscoreCase(modelName));
        return designModel;
    }

    /**
     * Extract new index, delete index records from index changes, update operations on indexName,
     * indexFields, uniqueIndex will also be converted to delete index and create index.
     * For scenarios where the model is not modified but only fields are added, modified, or deleted,
     * attach these fields to a newly created design model object.
     *
     * @param indexChanges Index change records, including add, modify, delete records
     * @return DDL SQL related to table structure
     */
    @Override
    public String generateIndexDDL(@NotNull ModelChangesDTO indexChanges) {
        StringBuilder ddl = new StringBuilder();
        if (indexChanges == null) {
            return ddl.toString();
        }
        List<DesignModelIndex> deletedIndexes = new ArrayList<>();
        List<DesignModelIndex> createdIndexes = new ArrayList<>();
        // Extract new index, delete index records from index change records
        for (RowChangeDTO rowChangeDTO : indexChanges.getDeletedRows()) {
            DesignModelIndex currentIndex = BeanTool.mapToObject(rowChangeDTO.getCurrentData(), DesignModelIndex.class);
            deletedIndexes.add(currentIndex);
        }
        for (RowChangeDTO rowChangeDTO : indexChanges.getCreatedRows()) {
            DesignModelIndex currentIndex = BeanTool.mapToObject(rowChangeDTO.getCurrentData(), DesignModelIndex.class);
            createdIndexes.add(currentIndex);
        }
        // Extract index update records
        for (RowChangeDTO rowChangeDTO : indexChanges.getUpdatedRows()) {
            DesignModelIndex currentIndex = BeanTool.mapToObject(rowChangeDTO.getCurrentData(), DesignModelIndex.class);
            if (!Collections.disjoint(INDEX_PROPERTIES, rowChangeDTO.getDataAfterChange().keySet())) {
                // In the case of index update records, when database-related properties INDEX_PROPERTIES are modified,
                // rebuild the index, i.e., split into delete index and create index
                Map<String, Object> previousData = new HashMap<>(rowChangeDTO.getCurrentData());
                previousData.putAll(rowChangeDTO.getDataBeforeChange());
                DesignModelIndex previousIndex = BeanTool.mapToObject(previousData, DesignModelIndex.class);
                deletedIndexes.add(previousIndex);
                createdIndexes.add(currentIndex);
            }
        }
        // Construct index DDL
        if (!deletedIndexes.isEmpty() || !createdIndexes.isEmpty()) {
            ddl.append("-- Alter table indexes:\n");
            ddl.append(DDLFactory.getInstance().alterTableIndexDDL(deletedIndexes, createdIndexes));
        }
        return ddl.toString();
    }

}