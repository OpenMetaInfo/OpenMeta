package info.openmeta.starter.designer.ddl;

import info.openmeta.starter.designer.entity.DesignField;
import info.openmeta.starter.designer.entity.DesignModel;
import info.openmeta.starter.designer.entity.DesignModelIndex;

import jakarta.validation.constraints.NotNull;
import java.util.List;

public interface DDLInterface {

    /**
     * DDL statement to create a table
     *
     * @param designModel Design model
     * @return Create table DDL statement
     */
    StringBuilder createTableDDL(DesignModel designModel);

    /**
     * DDL statement to drop a table
     *
     * @param tableName Table name
     * @return Drop table DDL statement
     */
    StringBuilder dropTableDDL(String tableName);

    /**
     * DDL statement to alter a table
     *
     * @param designModel Design model
     * @param deleteFields List of fields to delete
     * @param createFields List of fields to create
     * @param updateFields List of fields to update
     * @return Alter table DDL statement
     */
    StringBuilder alterTableDDL(DesignModel designModel, List<DesignField> deleteFields, List<DesignField> createFields, List<DesignField> updateFields);

    /**
     * DDL statement to alter indexes, including deleting and creating indexes
     */
    StringBuilder alterTableIndexDDL(@NotNull List<DesignModelIndex> deletedIndexes, @NotNull List<DesignModelIndex> createdIndexes);
}