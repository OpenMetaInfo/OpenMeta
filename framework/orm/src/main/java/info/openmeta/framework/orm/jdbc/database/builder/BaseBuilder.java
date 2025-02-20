package info.openmeta.framework.orm.jdbc.database.builder;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.domain.AggFunctions;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.jdbc.database.SqlWrapper;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Base Builder
 */
public abstract class BaseBuilder {
    protected final String mainModelName;
    protected final SqlWrapper sqlWrapper;
    protected final FlexQuery flexQuery;

    protected BaseBuilder(SqlWrapper sqlWrapper, FlexQuery flexQuery) {
        this.mainModelName = sqlWrapper.getModelName();
        this.sqlWrapper = sqlWrapper;
        this.flexQuery = flexQuery;
    }

    /**
     * Parse logic field, support OneToOne and ManyToOne cascade fields, such as deptId.managerId.name.
     * The cascade level does not exceed 3 levels (BaseConstant.CASCADE_LEVEL).
     * Note: OneToMany and ManyToMany fields are not supported here.
     *
     * @param logicField logic field
     * @param isSelect   whether it is a select field
     * @return field with alias, used for orderBy ..., such as t1.description
     */
    protected String parseLogicField(String logicField, boolean isSelect) {
        Assert.notBlank(logicField, "The logic field cannot be empty!");
        if (!logicField.contains(".")) {
            // Non-cascade field, directly add the main table alias and return
            return this.parseStoredField(logicField, isSelect);
        }
        List<String> cascadeFields = Arrays.asList(StringUtils.split(logicField, "."));
        // Due to database performance considerations, the cascade field cannot exceed the limited level!
        Assert.isTrue(cascadeFields.size() - 1 <= BaseConstant.CASCADE_LEVEL, "The cascade field {0} cannot exceed the {1} level!", logicField, BaseConstant.CASCADE_LEVEL);
        String modelName = this.mainModelName;
        String lastAlias = SqlWrapper.MAIN_TABLE_ALIAS;
        StringBuilder fieldChain = new StringBuilder(cascadeFields.getFirst());
        MetaField metaField;
        String columnAlias = "";
        for (int i = 0; i < cascadeFields.size(); i++) {
            String fieldName = cascadeFields.get(i);
            sqlWrapper.accessModelField(modelName, fieldName);
            metaField = ModelManager.getModelField(modelName, fieldName);
            columnAlias = lastAlias + "." + metaField.getColumnName();
            FieldType fieldType = metaField.getFieldType();
            if (FieldType.RELATED_TYPES.contains(fieldType)) {
                // Update the model, and continue to traverse the field list to update the model field access list
                modelName = metaField.getRelatedModel();
                // Exclude OneToMany/ManyToMany fields of the cascade field, only retain OneToOne/ManyToOne for calculating the alias of the last field
                Assert.isTrue(!FieldType.TO_MANY_TYPES.contains(fieldType), "Cannot read OneToMany/ManyToMany field by cascading, or sort by it {1}", logicField, fieldName);
                if (i < cascadeFields.size() - 1) {
                    // If the current field is not the last field, then the alias of the current field associated table
                    // is the leftAlias of the next level associated field.
                    String chainString = fieldChain.toString();
                    String rightAlias = sqlWrapper.getTableAlias().getRightTableAlias(chainString);
                    if (StringUtils.isBlank(rightAlias)) {
                        rightAlias = sqlWrapper.getTableAlias().generateRightTableAlias(chainString);
                        sqlWrapper.leftJoin(metaField, lastAlias, rightAlias, flexQuery.isAcrossTimeline());
                    }
                    // Update the last alias and field chain
                    lastAlias = rightAlias;
                    fieldChain.append(".").append(fieldName);
                }
            } else {
                // Only the last field allows non-relational fields, including the case where the cascade field has only one field
                Assert.isTrue(i == cascadeFields.size() - 1,
                        "The {0} field in cascade field {1} is not a relational field!", fieldName, logicField);
                if (metaField.isTranslatable()) {
                    // Construct the SQL segment of the translation field
                    columnAlias = sqlWrapper.selectTranslatableField(metaField, lastAlias, isSelect);
                }
            }
        }
        return columnAlias;
    }

    /**
     * Parse stored fields in batch
     * @param storedFields stored fields list
     * @param isSelect whether it is a select field
     * @return fields list with alias, used for select ..., groupBy ..., orderBy ...
     */
    protected List<String> parseStoredFields(List<String> storedFields, boolean isSelect) {
        return storedFields.stream().map(f -> this.parseLogicField(f, isSelect)).collect(Collectors.toList());
    }

    /**
     * Parse a stored field, may be a single field, or an alias of aggregation function query.
     *
     * @param storedField stored field name
     * @param isSelect    whether it is a select field
     * @return field with alias, used for select ..., groupBy ..., orderBy ...
     */
    private String parseStoredField(String storedField, boolean isSelect) {
        if (ModelManager.existField(this.mainModelName, storedField)) {
            MetaField metaField = ModelManager.getModelField(this.mainModelName, storedField);
            sqlWrapper.accessModelField(this.mainModelName, storedField);
            if (metaField.isTranslatable()) {
                // Construct the SQL segment of the translation field
                return sqlWrapper.selectTranslatableField(metaField, SqlWrapper.MAIN_TABLE_ALIAS, isSelect);
            } else {
                return SqlWrapper.MAIN_TABLE_ALIAS + "." + metaField.getColumnName();
            }
        } else if (AggFunctions.containAlias(this.flexQuery.getAggFunctions(), storedField)) {
            return storedField;
        } else {
            throw new IllegalArgumentException("Model {0} not exists Stored field {1}!", this.mainModelName, storedField);
        }
    }
}
