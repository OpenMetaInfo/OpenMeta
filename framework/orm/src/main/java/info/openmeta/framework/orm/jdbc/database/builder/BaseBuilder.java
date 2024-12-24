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
     * Parse logic fields in batch
     * @param logicFields logic fields list
     * @return fields list with alias, used for select ..., groupBy ..., orderBy ...
     */
    protected List<String> parseLogicFields(List<String> logicFields) {
        return logicFields.stream().map(this::parseLogicField).collect(Collectors.toList());
    }

    /**
     * Parse logic field, support OneToOne and ManyToOne cascade fields, such as deptId.managerId.name.
     * The cascade level does not exceed 3 levels (BaseConstant.CASCADE_LEVEL)
     *
     * @param logicField logic field
     * @return field with alias, used for orderBy ..., such as t1.description
     */
    protected String parseLogicField(String logicField) {
        Assert.notBlank(logicField, "The logic field cannot be empty!");
        if (!logicField.contains(".")) {
            // Non-cascade field, directly add the main table alias and return
            return this.parseSimpleField(logicField);
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
                    String rightAlias = sqlWrapper.getTableAlias().getRightTableAlias(fieldChain.toString());
                    sqlWrapper.leftJoin(metaField, lastAlias, rightAlias, flexQuery.isAcrossTimeline());
                    lastAlias = rightAlias;
                    fieldChain.append(".").append(fieldName);
                }
            } else {
                // Only the last field allows non-relational fields, including the case where the cascade field has only one field
                Assert.isTrue(i == cascadeFields.size() - 1,
                        "The {0} field in cascade field {1} is not a relational field!", fieldName, logicField);
                if (metaField.isTranslatable()) {
                    // Construct the SQL segment of the translation field
                    columnAlias = sqlWrapper.selectTranslatableField(metaField, lastAlias);
                }
            }
        }
        return columnAlias;
    }

    /**
     * Construct the SQL segment of the translation field
     *      COALESCE(NULLIF(trans.column_name, ''), t.column_name) AS column_name
     * Use the original value if the translation field is null or empty.
     *
     * @param leftAlias        left table alias
     * @param columnName       column name
     * @param transTableAlias translation table alias
     * @return SQL segment of the translation field
     */
    private String constructTransColumn(String leftAlias, String columnName, String transTableAlias) {
        String columnAlias = leftAlias + "." + columnName;
        return "COALESCE(NULLIF(" + transTableAlias + "." + columnName + ", ''), " +
                columnAlias + ") AS " + columnName;
    }

    /**
     * Parse a simple field, may be a single field, or an alias of aggregation function query.
     *
     * @param simpleField simple field
     * @return field with alias, used for select ..., groupBy ..., orderBy ...
     */
    private String parseSimpleField(String simpleField) {
        if (ModelManager.existField(this.mainModelName, simpleField)) {
            MetaField metaField = ModelManager.getModelField(this.mainModelName, simpleField);
            sqlWrapper.accessModelField(this.mainModelName, simpleField);
            if (metaField.isTranslatable()) {
                // Construct the SQL segment of the translation field
                return sqlWrapper.selectTranslatableField(metaField, SqlWrapper.MAIN_TABLE_ALIAS);
            } else {
                return SqlWrapper.MAIN_TABLE_ALIAS + "." + metaField.getColumnName();
            }
        } else if (AggFunctions.containAlias(this.flexQuery.getAggFunctions(), simpleField)) {
            return simpleField;
        } else {
            throw new IllegalArgumentException("Model {0} not exists field {1}!", this.mainModelName, simpleField);
        }
    }
}
