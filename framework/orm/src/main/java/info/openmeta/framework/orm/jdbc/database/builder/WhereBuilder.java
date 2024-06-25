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

import java.io.Serializable;
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
        // Multi-tenant model, add tenant filtering conditions
        Filters filters = this.handleMultiTenant(flexQuery.getFilters());
        // filters
        if (!Filters.isEmpty(filters)) {
            sqlWrapper.where(handleFilters(filters));
        }
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
        Serializable tenantId = ContextHolder.getContext().getTenantId();
        Filters tenantFilter = Filters.eq(ModelConstant.TENANT_ID, tenantId);
        return Filters.merge(filters, tenantFilter);
    }

    /**
     * If the field chain contains OneToMany or ManyToMany fields, construct the FlexQuery of the associated model,
     * then, call the search method to return the ids of the left model.
     * The final query condition is [id, in, ids].
     * For example:
     *      [deptId.empIds, in, [1,2,3]] is converted to [deptId.id, in, [4,5,6]], exchange empIds with deptIds.
     *
     * @param filters filters
     * @return sql condition of where clause
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
     * @return sql condition of where clause
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
     * @return sql condition fragment of a child node
     */
    private StringBuilder processChildFilter(Filters child, FilterXToManyParser xToManyBuilder) {
        if (FilterType.LEAF.equals(child.getType())) {
            // When child is a LEAF node, get the filterUnit object
            return this.parseFilterUnit(child.getFilterUnit(), xToManyBuilder);
        } else {
            // When child is a TREE node, recursively process, get the sql fragment of child node, and wrap it in ()
            StringBuilder treeSql = handleFiltersTree(child);
            if (!treeSql.isEmpty()) {
                return new StringBuilder("(").append(treeSql).append(")");
            }
        }
        return new StringBuilder();
    }

    /**
     * Parse FilterUnit to generate sql condition fragment.
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
            // Cascade field processing: f0.f1.f2.f3
            return this.parseCascadeField(filterUnit, xToManyBuilder);
        } else {
            // Non-cascade field processing: t.column = ?
            MetaField metaField = ModelManager.getModelField(mainModelName, logicField);
            sqlWrapper.accessModelField(mainModelName, logicField);
            // For non-relational fields, convert the filterUnit operator and query value.
            this.convertFilterUnit(metaField, filterUnit);
            return FilterUnitParser.parse(sqlWrapper, SqlWrapper.MAIN_TABLE_ALIAS, metaField, filterUnit);
        }
    }

    /**
     * Cascade field processing: f0.f1.f2.f3, construct join query statement.
     *
     * @param filterUnit Single filter unit
     * @param xToManyBuilder XToMany query condition builder at the same level
     * @return sql condition fragment of cascade field
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
            FieldType fieldType = metaField.getFieldType();
            if (FieldType.RELATED_TYPES.contains(metaField.getFieldType())) {
                // The processing of relational fields
                if (FieldType.TO_MANY_TYPES.contains(fieldType)) {
                    // Truncate OneToMany/ManyToMany fields.
                    // If there are still cascading fields behind, use the second half as the new FilterUnit field.
                    // If the current field is the last field, use "id" as the new FilterUnit field,
                    // which is the id field of the right model.
                    String fieldsOfMany = i < size - 1 ? String.join(".", cascadeFields.subList(i + 1, size)) : ModelConstant.ID;
                    FilterUnit newUnit = new FilterUnit(fieldsOfMany, filterUnit.getOperator(), filterUnit.getValue());
                    xToManyBuilder.addXToManyFilterUnit(currentAlias, metaField, newUnit);
                    // XToMany fields are temporarily stored and an empty StringBuilder object is returned directly,
                    // and the sql is processed separately
                    return new StringBuilder();
                } else if (i < size - 1) {
                    // Current xToOne field is not the last field, add it as a leftJoin condition.
                    String fieldChain = String.join(".", cascadeFields.subList(0, i + 1));
                    String rightAlias = sqlWrapper.getRightTableAliasByFieldChain(fieldChain);
                    sqlWrapper.leftJoin(metaField, currentAlias, rightAlias, flexQuery.isAcrossTimeline());
                    // The alias of the associated table of current xToOne field, is used as the currentAlias for next loop
                    currentAlias = rightAlias;
                    // The associated model of current xToOne field, is used as the currentModelName for next loop
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
     * @return `OR` sql conditions based on searchName, such as: (c1 or c2)
     */
    private StringBuilder parseSearchName(FilterUnit filterUnit, FilterXToManyParser xToManyBuilder) {
        List<String> searchNames = ModelManager.getModel(mainModelName).getSearchName();
        Assert.notEmpty(searchNames,
                "The `searchName` of the model {0} is empty and cannot be used as a query condition!",
                mainModelName);
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
     * Convert the operator and value of the filterUnit.
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
            if (Operator.NOT_HAS.equals(operator) || Operator.NOT_START_WITH.equals(operator)) {
                operator = Operator.NOT_EQUAL;
            } else if (Operator.HAS.equals(operator) || Operator.START_WITH.equals(operator)) {
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
