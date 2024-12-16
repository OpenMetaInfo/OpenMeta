package info.openmeta.starter.designer.version;

import info.openmeta.starter.designer.dto.ModelChangesDTO;

/**
 * Version control DDL generation, including table structure and indexes
 */
public interface VersionDdl {
    /**
     * Analyze and construct table structure DDL statements based on model and field change records
     *
     * @param modelChanges Model change records, including add, modify, delete records
     * @param fieldChanges Field change records, including add, modify, delete records
     * @return DDL SQL related to table structure
     */
    String generateTableDDL(ModelChangesDTO modelChanges, ModelChangesDTO fieldChanges);

    /**
     * Extract new index, delete index records from index change records, update operations on indexName,
     * indexFields, uniqueIndex will also be converted to delete index and create index.
     * For scenarios where the model is not modified but only fields are added, modified, or deleted,
     * attach these fields to a newly created design model object.
     *
     * @param indexChangeList Index change records, including add, modify, delete records
     * @return DDL SQL related to table structure
     */
    String generateIndexDDL(ModelChangesDTO indexChangeList);
}