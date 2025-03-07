package info.openmeta.framework.orm.jdbc.database.builder;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.FilterUnit;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.encrypt.EncryptUtils;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.enums.FilterType;
import info.openmeta.framework.orm.jdbc.database.SqlWrapper;
import info.openmeta.framework.orm.jdbc.database.parser.FilterUnitParser;
import info.openmeta.framework.orm.jdbc.database.parser.FilterXToManyParser;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

/**
 * WHERE clause builder
 */
public class WhereBuilder extends BaseBuilder implements SqlClauseBuilder {

    public WhereBuilder(SqlWrapper sqlWrapper, FlexQuery flexQuery) {
        super(sqlWrapper, flexQuery);
    }

    public void build() {
        // SoftDelete filter processing
        Filters filters = this.handleSoftDeleted(flexQuery.getFilters());
        // Access control filter processing
        filters = this.handleActiveControl(filters);
        // Multi-tenant model, add tenant filtering conditions
        filters = this.handleMultiTenant(filters);
        // filters
        if (!Filters.isEmpty(filters)) {
            sqlWrapper.where(handleFilters(filters));
        }
    }

    /**
     * Update filters based on the softDelete config of the model.
     * If the model is soft-deleted, and filters do not contain the `disabled` field,
     * append the ["disabled", "=", true] filtering condition to filters.
     *
     * @param filters original filters
     * @return processed filters
     */
    private Filters handleSoftDeleted(Filters filters) {
        if (!ModelManager.isSoftDeleted(mainModelName)) {
            // The model is not soft-deleted, return directly
            return filters;
        }
        String softDeleteField = ModelManager.getSoftDeleteField(mainModelName);
        if (Filters.containsField(filters, softDeleteField)) {
            // The filters already contain the softDelete field, return directly
            return filters;
        }
        Filters deletedFilters = new Filters().eq(softDeleteField, false);
        // Merge the original filters and the softDelete filters ["disabled", "=", true]
        return Filters.and(filters, deletedFilters);
    }

    /**
     * Update filters based on the active control config of the model.
     * If the model is active-controlled, and filters do not contain the `active` field,
     * append the ["active", "=", true] filtering condition to filters.
     *
     * @param filters original filters
     * @return processed filters
     */
    private Filters handleActiveControl(Filters filters) {
        if (!ModelManager.isEnableActiveControl(mainModelName)) {
            // The model does not enable active control, return directly
            return filters;
        }
        if (Filters.containsField(filters, ModelConstant.ACTIVE_CONTROL_FIELD)) {
            // The filters already contain the `active` field, return directly
            return filters;
        }
        // Add active control filtering conditions
        Filters activeControlFilter = new Filters().eq(ModelConstant.ACTIVE_CONTROL_FIELD, true);
        return Filters.and(filters, activeControlFilter);
    }

    /**
     * Handle tenant filtering conditions for multi-tenant models.
     *
     * @param filters original filters
     * @return processed filters
     */
    private Filters handleMultiTenant(Filters filters) {
        if (!ModelManager.isMultiTenant(mainModelName)) {
            // The model is not multi-tenant, return directly
            return filters;
        }
        // Add tenant filtering conditions
        String tenantId = ContextHolder.getContext().getTenantId();
        Filters tenantFilter = new Filters().eq(ModelConstant.TENANT_ID, tenantId);
        return Filters.and(filters, tenantFilter);
    }

    /**
     * If the field chain contains OneToMany or ManyToMany fields, construct the FlexQuery of the associated model,
     * then, call the search method to return the ids of the left model.
     * The final query condition is [id, in, ids].
     * For example,
     *      [deptId.empIds, in, [1,2,3]] is converted to [deptId.id, in, [4,5,6]], exchange empIds with deptIds.
     *
     * @param filters filters
     * @return SQL condition of where clause
     */
    private StringBuilder handleFilters(Filters filters) {
        if (FilterType.LEAF.equals(filters.getType())) {
            // For LEAF type, only one FilterUnit, which may be XToMany field, directly process the conditions of this layer.
            FilterXToManyParser xToManyBuilder = new FilterXToManyParser(mainModelName, sqlWrapper, filters.getLogicOperator());
            StringBuilder sqlBuilder = this.parseFilterUnit(filters.getFilterUnit(), xToManyBuilder);
            if (!xToManyBuilder.isEmpty()) {
                // xToManyBuilder is not empty, construct XToMany field query conditions
                sqlBuilder.append(xToManyBuilder.parse());
            }
            return sqlBuilder;
        } else if (FilterType.TREE.equals(filters.getType())) {
            return handleFiltersTree(filters);
        }
        throw new BusinessException("Filters property exception! {0}", filters);
    }

    /**
     * Handle the TREE Filters recursively.
     *
     * @param filters TREE filters object
     * @return SQL condition of where clause
     */
    private StringBuilder handleFiltersTree(Filters filters) {
        // Current level filter information
        FilterXToManyParser xToManyBuilder = new FilterXToManyParser(mainModelName, sqlWrapper, filters.getLogicOperator());
        StringBuilder sqlBuilder = new StringBuilder();
        String logic = filters.getLogicOperator().name();
        for (Filters child : filters.getChildren()) {
            StringBuilder childSql = processChildFilter(child, xToManyBuilder);
            if (!sqlBuilder.isEmpty() && !childSql.isEmpty()) {
                // Add logical operator `AND` or `OR` for the same level of filters.
                sqlBuilder.append(" ").append(logic).append(" ");
            }
            sqlBuilder.append(childSql);
        }
        // Construct XToMany field query conditions based on xToManyBuilder
        if (!xToManyBuilder.isEmpty()) {
            StringBuilder xToMSql = xToManyBuilder.parse();
            if (!sqlBuilder.isEmpty() && !xToMSql.isEmpty()) {
                sqlBuilder.append(" ").append(logic).append(" ");
            }
            sqlBuilder.append(xToMSql);
        }
        return sqlBuilder;
    }

    /**
     * Process a child node of the filters object, which may be LEAF type or TREE type.
     *
     * @param child child node of filters
     * @param xToManyBuilder XToMany query condition builder at the same level
     * @return SQL condition fragment of a child node
     */
    private StringBuilder processChildFilter(Filters child, FilterXToManyParser xToManyBuilder) {
        if (FilterType.LEAF.equals(child.getType())) {
            // When a child is a LEAF node, get the filterUnit object
            return this.parseFilterUnit(child.getFilterUnit(), xToManyBuilder);
        } else {
            // When child is a TREE node, recursively process, get the SQL fragment of child node, and wrap it in ()
            StringBuilder treeSql = handleFiltersTree(child);
            if (!treeSql.isEmpty()) {
                return new StringBuilder("(").append(treeSql).append(")");
            }
        }
        return new StringBuilder();
    }

    /**
     * Parse FilterUnit to generate SQL condition fragment.
     *
     * @param filterUnit Single filter unit
     * @param xToManyBuilder XToMany query condition builder at the same level.
     */
    private StringBuilder parseFilterUnit(FilterUnit filterUnit, FilterXToManyParser xToManyBuilder) {
        String logicField = filterUnit.getField();
        if (ModelConstant.SEARCH_NAME.equals(logicField)) {
            // Search by main model `searchName` directly.
            return this.parseSearchName(filterUnit, xToManyBuilder);
        } else if (logicField.contains(".")) {
            // Cascade field processing `f0.f1.f2.f3`
            return this.parseCascadeField(filterUnit, xToManyBuilder);
        } else {
            // Single model field processing
            MetaField metaField = ModelManager.getModelField(mainModelName, logicField);
            sqlWrapper.accessModelField(mainModelName, logicField);
            if (FieldType.TO_MANY_TYPES.contains(metaField.getFieldType())) {
                // XToMany field processing, such as `["projectIds", "IN", [20]]`, execute `id IN (20)` on the right model.
                FilterUnit newUnit = new FilterUnit(ModelConstant.ID, filterUnit.getOperator(), filterUnit.getValue());
                xToManyBuilder.addXToManyFilterUnit(SqlWrapper.MAIN_TABLE_ALIAS, metaField, newUnit);
                return new StringBuilder();
            } else {
                // Non-cascade field processing: `t.column = ?`
                this.convertFilterUnit(metaField, filterUnit);
                return FilterUnitParser.parse(sqlWrapper, SqlWrapper.MAIN_TABLE_ALIAS, metaField, filterUnit);
            }
        }
    }

    /**
     * Cascade field processing: f0.f1.f2.f3, construct a join query statement.
     *
     * @param filterUnit Single filter unit
     * @param xToManyBuilder XToMany query condition builder at the same level
     * @return SQL condition fragment of cascade field
     */
    private StringBuilder parseCascadeField(FilterUnit filterUnit, FilterXToManyParser xToManyBuilder) {
        List<String> cascadeFields = Arrays.asList(StringUtils.split(filterUnit.getField(), "."));
        Assert.notTrue(cascadeFields.contains(ModelConstant.SEARCH_NAME),
                "The model {0} does not support search with a cascading searchName: {1}", mainModelName, filterUnit.getField());
        int size = cascadeFields.size();
        Assert.isTrue(size - 1 <= BaseConstant.CASCADE_LEVEL,
                "Based on database performance considerations, the cascading query of model {0} cannot exceed the {1} level: {2}!",
                mainModelName, BaseConstant.CASCADE_LEVEL, filterUnit.getField());
        String currentAlias = SqlWrapper.MAIN_TABLE_ALIAS;
        String currentModelName = mainModelName;
        MetaField metaField = null;
        for (int i = 0; i < size; i++) {
            String fieldName = cascadeFields.get(i);
            sqlWrapper.accessModelField(currentModelName, fieldName);
            metaField = ModelManager.getModelField(currentModelName, fieldName);
            if (FieldType.RELATED_TYPES.contains(metaField.getFieldType())) {
                // The processing of relational fields
                if (FieldType.TO_MANY_TYPES.contains(metaField.getFieldType())) {
                    // Truncate OneToMany/ManyToMany fields.
                    // If there are still cascading fields behind, use the second half as the new FilterUnit field.
                    // If the current field is the last field, use "id" as the new FilterUnit field,
                    // which is the id field of the right model.
                    String fieldsOfManyEndpoint = i < size - 1 ?
                            String.join(".", cascadeFields.subList(i + 1, size)) : ModelConstant.ID;
                    FilterUnit newUnit = new FilterUnit(fieldsOfManyEndpoint, filterUnit.getOperator(), filterUnit.getValue());
                    xToManyBuilder.addXToManyFilterUnit(currentAlias, metaField, newUnit);
                    // XToMany fields are temporarily stored, and an empty StringBuilder object is returned directly,
                    // and the SQL is processed separately
                    return new StringBuilder();
                } else if (i < size - 1) {
                    // The Current xToOne field is not the last field, add it as a leftJoin condition.
                    String fieldChain = String.join(".", cascadeFields.subList(0, i + 1));
                    String rightAlias = sqlWrapper.getTableAlias().getRightTableAlias(fieldChain);
                    if (StringUtils.isBlank(rightAlias)) {
                        // If the associated table alias does not exist, generate a new alias and add a leftJoin condition.
                        rightAlias = sqlWrapper.getTableAlias().generateRightTableAlias(fieldChain);
                        sqlWrapper.leftJoin(metaField, currentAlias, rightAlias, flexQuery.isAcrossTimeline());
                    }
                    // The alias of the associated table for current xToOne field is used as the currentAlias for next loop
                    currentAlias = rightAlias;
                    // The associated model of current xToOne field is used as the currentModelName for next loop
                    currentModelName = metaField.getRelatedModel();
                }
            } else {
                // For non-relational fields, when i < size - 1, it means that there is a cascading query through non-relational fields.
                Assert.isTrue(i == size - 1,
                        "For the cascading query of model {0}, the field {1} in cascade string {2} is not a relational field!",
                        mainModelName, fieldName, filterUnit.getField());
                // Convert the filterUnit operator and query value for non-relational fields.
                this.convertFilterUnit(metaField, filterUnit);
            }
        }
        // When there is no XToMany field, get the field alias normally,
        // including the case where the last field is ManyToOne/OneToOne field.
        return FilterUnitParser.parse(sqlWrapper, currentAlias, metaField, filterUnit);
    }

    /**
     * When using the virtual field `searchName` for searching,
     * it is automatically split into multiple fields for `OR` condition search.
     * Only the main mode supports using `searchName` for searching.
     *
     * @param filterUnit Single filter unit
     * @param xToManyBuilder XToMany query condition builder at the same level
     * @return `OR` SQL conditions based on searchName, such as (c1 or c2)
     */
    private StringBuilder parseSearchName(FilterUnit filterUnit, FilterXToManyParser xToManyBuilder) {
        List<String> searchNames = ModelManager.getModel(mainModelName).getSearchName();
        Assert.notEmpty(searchNames,
                "The `searchName` of model {0} is not configured and cannot be used in filter {1}",
                mainModelName, filterUnit.toString());
        searchNames.forEach(f -> sqlWrapper.accessModelField(mainModelName, f));
        // Construct the `OR` condition search based on the `searchName` fields.
        Operator operator = filterUnit.getOperator();
        Object value = filterUnit.getValue();
        StringBuilder filterUnitSql = new StringBuilder();
        if (searchNames.size() == 1) {
            // When there is only one field configured in `searchName`, parse directly based on this field.
            FilterUnit newUnit = new FilterUnit(searchNames.getFirst(), operator, value);
            filterUnitSql.append(parseFilterUnit(newUnit, xToManyBuilder));
        } else {
            // When there are multiple fields configured in `searchName`,
            // split into multiple fields for `OR` condition search.
            filterUnitSql.append(" (");
            for (int i = 0; i < searchNames.size(); i++) {
                if (i > 0) {
                    filterUnitSql.append(" OR ");
                }
                FilterUnit newUnit = new FilterUnit(searchNames.get(i), operator, value);
                filterUnitSql.append(parseFilterUnit(newUnit, xToManyBuilder));
            }
            filterUnitSql.append(") ");
        }
        return filterUnitSql;
    }

    /**
     * Convert the operator and value of the filterUnit based on the field type for non-relational fields.
     *
     * @param metaField field object
     * @param filterUnit filterUnit
     */
    private void convertFilterUnit(MetaField metaField, FilterUnit filterUnit) {
        // Skip directly if the value is null
        if (filterUnit.getValue() == null) {
            return;
        }
        if (metaField.isEncrypted()) {
            // If the current field is an encrypted field, encrypt the search value,
            // and reset the operator to "=" or "!=", for accurate search.
            Operator operator = filterUnit.getOperator();
            Object value = filterUnit.getValue();
            if (Operator.NOT_CONTAINS.equals(operator) || Operator.NOT_START_WITH.equals(operator)) {
                operator = Operator.NOT_EQUAL;
            } else if (Operator.CONTAINS.equals(operator) || Operator.START_WITH.equals(operator)) {
                operator = Operator.EQUAL;
            }
            if (value instanceof String) {
                value = EncryptUtils.encrypt((String) value);
            } else if (value instanceof Collection) {
                value = ((Collection<?>) value).stream().filter(v -> StringUtils.isNotBlank((String) v))
                        .map(v -> EncryptUtils.encrypt((String) v)).collect(Collectors.toList());
            }
            filterUnit.setOperator(operator);
            filterUnit.setValue(value);
        }
    }
}
