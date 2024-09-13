package info.openmeta.framework.orm.jdbc.database.builder;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.orm.domain.AggFunctions;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.enums.AggFunctionType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.jdbc.database.SqlWrapper;
import info.openmeta.framework.base.utils.Assert;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Build aggregation query conditions
 * Aggregate builder includes groupBy processing, so it must precede the orderBy processing.
 */
public class AggregateBuilder extends BaseBuilder implements SqlClauseBuilder {

    public AggregateBuilder(SqlWrapper sqlWrapper, FlexQuery flexQuery) {
        super(sqlWrapper, flexQuery);
    }

    public void build() {
        if (flexQuery.isAggregate()) {
            handleAggregate();
        }
    }

    /**
     * Process aggregate queries, row grouping and column grouping conditions,
     * and automatically perform sum() and count() operations on numeric fields
     */
    private void handleAggregate() {
        // Handle aggregate function queries for fields
        handleAggFunctions(flexQuery.getAggFunctions());
        // Handle groupBy fields, only add row grouping fields and column grouping fields to the select fields,
        // and extract the numeric fields that need to be summed up.
        List<String> sqlGroupByFields = new ArrayList<>(flexQuery.getGroupBy());
        sqlGroupByFields.addAll(flexQuery.getSplitBy());
        if (!CollectionUtils.isEmpty(sqlGroupByFields)) {
            // Grouping fields can only be stored fields
            ModelManager.validateStoredFields(this.mainModelName, sqlGroupByFields);
            // Add grouping fields to the select condition and extract the numeric field set
            this.handleGroupByFields(flexQuery, sqlGroupByFields);
            sqlWrapper.groupBy(this.parseLogicFields(sqlGroupByFields));
        }
    }

    /**
     * Process groupBy query fields, only add row grouping fields and column grouping fields to the select fields,
     * and extract the numeric fields that need to be summed up.
     *
     * @param flexQuery flexQuery
     * @param sqlGroupByFields group By fields list in SQL query, row grouping and column grouping fields
     */
    private void handleGroupByFields(FlexQuery flexQuery, List<String> sqlGroupByFields) {
        Set<String> numericFields = new HashSet<>();
        Set<String> selectFields = new HashSet<>(sqlGroupByFields);
        if (!CollectionUtils.isEmpty(flexQuery.getFields())) {
            // If flexQuery.fields is specified, only retain numeric fields that appear in fields
            numericFields = ModelManager.getModelStoredNumericFields(this.mainModelName);
            numericFields.retainAll(flexQuery.getFields());
            // Remove numeric fields that appear in grouping conditions to avoid groupBy conditions being calculated by `sum()`
            numericFields.removeAll(new HashSet<>(sqlGroupByFields));
            selectFields.addAll(flexQuery.getFields());
            selectFields.removeAll(numericFields);
        }
        // Update flexQuery.fields
        flexQuery.setFields(selectFields);
        // Add select fields to groupBy fields to incompatible with `sql_mode = only_full_group_by`
        selectFields.stream().filter(f -> !sqlGroupByFields.contains(f)).forEach(sqlGroupByFields::add);
        // Reassign selectFields, only select non-numeric, stored fields of the current model,
        // excluding cascading query fields (a.b.c), non-stored fields,
        // numeric fields that are excluded will be aggregated by `sum()`,
        // and other fields will be filled in the return value format.
        List<String> storedFields = selectFields.stream()
                .filter(f -> ModelManager.existField(mainModelName, f) && ModelManager.isStored(mainModelName, f))
                .collect(Collectors.toList());
        sqlWrapper.select(this.parseLogicFields(storedFields));
        // Numeric fields, automatically add `sum(t.field) as field`, the alias here cannot add table alias
        if (!CollectionUtils.isEmpty(numericFields)) {
            numericFields.forEach(field -> {
                sqlWrapper.accessModelField(this.mainModelName, field);
                String column = ModelManager.getModelFieldColumn(this.mainModelName, field);
                String alias = "sum" + Character.toUpperCase(field.charAt(0)) + field.substring(1);
                sqlWrapper.select("SUM(" + SqlWrapper.MAIN_TABLE_ALIAS + "." + column + ") AS " + alias);
            });
        }
        // Automatically add `count(*) as count` for aggregation queries
        sqlWrapper.count();
    }

    /**
     * Process aggregate function fields, automatically add 'SUM(t.field) AS sumField'
     * Currently only supports aggregate function queries on the main table
     *
     * @param aggFunctions aggregate function fields
     */
    private void handleAggFunctions(AggFunctions aggFunctions) {
        if (AggFunctions.isEmpty(aggFunctions)) {
            return;
        }
        aggFunctions.getFunctionList().forEach(aggFunction -> {
            String field = aggFunction.getField();
            sqlWrapper.accessModelField(this.mainModelName, field);
            MetaField metaField = ModelManager.getModelField(this.mainModelName, field);
            Assert.notTrue(metaField.isDynamic(),
                    "Model field {0}:{1} is a dynamic field, and cannot be used for aggregate function queries!",
                    this.mainModelName, field);
            // Verify that the field type matches the aggregate function type
            boolean isValid = AggFunctionType.validateFunctionType(aggFunction.getType(), metaField.getFieldType());
            Assert.isTrue(isValid, "Aggregate function {0} does not support field type: {1}",
                    aggFunction, metaField.getFieldType().getType());
            String selectSegment = this.generateAggSegment(aggFunction.getType(), field, metaField);
            sqlWrapper.select(selectSegment);
        });
    }

    /**
     * Generate aggregation segment.
     * Scenarios that do not need to deal with decimals, such as `SUM`, `COUNT`, `MAX`, `MIN`.
     *      SUM(column_name) AS fieldAlias
     * Scenarios that need to deal with decimals, such as `AVG`, `ROUND(AVG(column_name), 2)`.
     *      ROUND(AVG(column_name), 2) AS fieldAlias
     *
     * @param type aggregation function type
     * @param field field name
     * @param metaField metaField
     * @return aggregation segment
     */
    private String generateAggSegment(AggFunctionType type, String field, MetaField metaField) {
        String alias = type.getFunc() + Character.toUpperCase(field.charAt(0)) + field.substring(1);
        // SUM(column_name)
        String selectSegment = type.name() + "(" + SqlWrapper.MAIN_TABLE_ALIAS + "." + metaField.getColumnName() + ")";
        if (AggFunctionType.AVG.equals(type)) {
            // ROUND(AVG(column_name), 2)
            int scale = metaField.getScale() == null || metaField.getScale() <= 0 ?
                    BaseConstant.DEFAULT_SCALE : metaField.getScale();
            selectSegment = "ROUND(" + selectSegment + ", " + scale + ")";
        }
        // `SUM(column_name) AS fieldAlias` or `ROUND(AVG(column_name), 2) AS fieldAlias`
        return selectSegment + " AS " + alias;
    }
}
