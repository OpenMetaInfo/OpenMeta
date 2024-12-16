package info.openmeta.starter.designer.ddl;

import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.starter.designer.entity.DesignField;
import info.openmeta.starter.designer.entity.DesignModel;
import info.openmeta.starter.designer.entity.DesignModelIndex;
import info.openmeta.framework.base.utils.Assert;
import org.springframework.util.CollectionUtils;

import jakarta.validation.constraints.NotNull;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static info.openmeta.framework.orm.constant.ModelConstant.ID;
import static info.openmeta.framework.orm.constant.ModelConstant.SLICE_ID;

/**
 * MySQL DDL dialect utility class
 */
public class MySQLDDL implements DDLInterface {

    /**
         CREATE TABLE product_info(
             id BIGINT(32) NOT NULL AUTO_INCREMENT  COMMENT 'ID' ,
             name VARCHAR(32) NOT NULL COMMENT 'Product name' ,
             PRIMARY KEY (id)
         ) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT = 'Product Info';
     *
     * @param designModel Model object containing the field list
     * @return Create table DDL
     */
    public StringBuilder createTableDDL(DesignModel designModel) {
        StringBuilder ddl = new StringBuilder("CREATE TABLE ").append(designModel.getTableName()).append(" (\n");
        Assert.notEmpty(designModel.getModelFields(), "The fields of the model {0} to be published cannot be empty!", designModel.getModelName());
        String pkColumn = Boolean.TRUE.equals(designModel.getTimeline()) ? SLICE_ID : ID;
        for (DesignField designField : designModel.getModelFields()) {
            ddl.append("    ").append(addColumnDDL(designField, pkColumn));
        }
        ddl.append("    PRIMARY KEY (").append(pkColumn).append(")\n");
        ddl.append(") ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='").append(designModel.getLabelName()).append("';\n\n");
        return ddl;
    }

    /**
         ALTER TABLE product_info
         MODIFY COLUMN name VARCHAR(32) NOT NULL COMMENT 'Name',
         ADD COLUMN description MEDIUMTEXT COMMENT 'Description';
     *
     * @param designModel Model object
     * @param deleteFields List of fields to delete
     * @param createFields List of fields to create
     * @param updateFields List of fields to update
     * @return Alter table DDL
     */
    public StringBuilder alterTableDDL(DesignModel designModel, List<DesignField> deleteFields, List<DesignField> createFields, List<DesignField> updateFields) {
        StringBuilder ddl = new StringBuilder("ALTER TABLE ").append(designModel.getTableName()).append("\n");
        String pkColumn = Boolean.TRUE.equals(designModel.getTimeline()) ? SLICE_ID : ID;
        // Renaming table is not supported currently
        if (!CollectionUtils.isEmpty(deleteFields)) {
            // Delete fields
            for (DesignField designField : deleteFields) {
                ddl.append(dropColumnDDL(designField));
            }
        }
        if (!CollectionUtils.isEmpty(createFields)) {
            // Add fields
            for (DesignField designField : createFields) {
                ddl.append("    ADD COLUMN ").append(addColumnDDL(designField, pkColumn));
            }
        }
        if (!CollectionUtils.isEmpty(updateFields)) {
            // Update fields
            for (DesignField designField : updateFields) {
                ddl.append(modifyColumnDDL(designField, pkColumn));
            }
        }
        addBlankLine(ddl);
        return ddl;
    }

    /**
     * DROP TABLE product_info;
     *
     * @param tableName Table name
     * @return Drop table DDL
     */
    public StringBuilder dropTableDDL(String tableName) {
        return new StringBuilder("DROP TABLE ").append(tableName).append(";\n");
    }

    /**
     * DDL for adding a field
     *      name VARCHAR(32) DEFAULT '' COMMENT 'Name',\n
     * @param designField Field object
     * @return Add field DDL
     */
    private StringBuilder addColumnDDL(DesignField designField, String pkColumn) {
        String dbType = MySQLDateType.getDbType(designField.getFieldType());
        StringBuilder ddl = new StringBuilder(designField.getColumnName()).append(" ").append(dbType);
        if (designField.getLength() != null && designField.getLength() > 0) {
            ddl.append("(").append(designField.getLength());
            if (designField.getScale() != null && designField.getScale() > 0) {
                ddl.append(",").append(designField.getScale());
            }
            ddl.append(")");
        }
        if (designField.getFieldName().equals(pkColumn)) {
            ddl.append(" AUTO_INCREMENT");
        }
        ddl.append(" COMMENT '").append(designField.getLabelName()).append("',\n");
        return ddl;
    }

    /**
     * DDL for modifying a field, if needed, can incrementally modify field attributes based on new data,
     * but must provide data_type, length, scale.
     *
     * @param designField Field object to modify
     * @param pkColumn Primary key field name
     */
    private StringBuilder modifyColumnDDL(DesignField designField, String pkColumn) {
        return new StringBuilder("    MODIFY COLUMN ").append(addColumnDDL(designField, pkColumn));
    }

    /**
     * DROP COLUMN dep_id,\n
     *
     * @param designField Design field object
     */
    private StringBuilder dropColumnDDL(DesignField designField) {
        Assert.notTrue(ID.equals(designField.getFieldName()), "Cannot delete id field!");
        return new StringBuilder("    DROP COLUMN ").append(designField.getColumnName()).append(",\n");
    }

    /**
     * DDL statement to alter indexes, including deleting and creating indexes.
     */
    public StringBuilder alterTableIndexDDL(@NotNull List<DesignModelIndex> deletedIndexes, @NotNull List<DesignModelIndex> createdIndexes) {
        StringBuilder ddl = new StringBuilder();
        Map<String, List<DesignModelIndex>> deletedIndexMap = deletedIndexes.stream().collect(Collectors.groupingBy(DesignModelIndex::getModelName));
        Map<String, List<DesignModelIndex>> createdIndexMap = createdIndexes.stream().collect(Collectors.groupingBy(DesignModelIndex::getModelName));
        deletedIndexMap.forEach((modelName, deletedModelIndexes) -> {
            ddl.append("ALTER TABLE ").append(StringTools.toUnderscoreCase(modelName)).append("\n");
            for (DesignModelIndex designModelIndex : deletedModelIndexes) {
                ddl.append("    DROP INDEX ").append(designModelIndex.getIndexName()).append(",\n");
            }
            // When the same model has both deleted and created indexes, merge into one Alter table DDL segment
            if (createdIndexMap.containsKey(modelName)) {
                ddl.append(addIndexDDL(createdIndexMap.get(modelName)));
            }
            addBlankLine(ddl);
        });
        // Models with only created indexes
        createdIndexMap.forEach((modelName, createdModelIndexes) -> {
            if (!deletedIndexMap.containsKey(modelName)) {
                ddl.append("ALTER TABLE ").append(StringTools.toUnderscoreCase(modelName)).append("\n");
                ddl.append(addIndexDDL(createdModelIndexes));
                addBlankLine(ddl);
            }
        });
        return ddl;
    }

    /**
     * DDL statement to add indexes
     */
    private StringBuilder addIndexDDL(List<DesignModelIndex> createdIndexes) {
        StringBuilder ddl = new StringBuilder();
        for (DesignModelIndex designModelIndex : createdIndexes) {
            ddl.append("    ADD ");
            if (Boolean.TRUE.equals(designModelIndex.getUniqueIndex())) {
                ddl.append("UNIQUE ");
            }
            ddl.append("INDEX ").append(designModelIndex.getIndexName()).append(",\n");
        }
        return ddl;
    }

    /**
     * When the end of the DDL segment is ",\n", replace it with ";\n\n"
     */
    private void addBlankLine(StringBuilder ddl) {
        String suffix = ",\n";
        if (ddl.toString().endsWith(suffix)) {
            ddl.delete(ddl.length() - suffix.length(), ddl.length());
            ddl.append(";\n\n");
        }
    }
}