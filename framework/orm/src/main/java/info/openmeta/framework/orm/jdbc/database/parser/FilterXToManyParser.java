package info.openmeta.framework.orm.jdbc.database.parser;

import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.FilterUnit;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.enums.LogicOperator;
import info.openmeta.framework.orm.jdbc.database.SqlWrapper;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.utils.ReflectTool;
import lombok.Data;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * FilterXToManyParser
 * Centralized processing of XToMany filtering conditions in the same level when parsing Filters.
 * Merge multiple filterUnits based on the same XToOne or XToMany field,
 * Field alias corresponding to the current level filterUnit, field relationship, xToMany filterUnit.
 */
public class FilterXToManyParser {
    private final String mainModel;
    private final SqlWrapper sqlWrapper;
    private final LogicOperator logicOperator;

    /**
     * The map of field alias to filter conditions, based on the same XToMany field,
     * fieldAlias format like: table_alias.column, t1.code.
     */
    private final Map<String, XToManyFieldFilters> xToManyFieldFiltersMap = new HashMap<>();

    /**
     * FilterUnit combination constructor.
     *
     * @param mainModel main model name
     * @param sqlWrapper sqlWrapper
     * @param logicOperator logical operator of the current level filters combination
     */
    public FilterXToManyParser(String mainModel, SqlWrapper sqlWrapper, LogicOperator logicOperator) {
        this.mainModel = mainModel;
        this.sqlWrapper = sqlWrapper;
        this.logicOperator = logicOperator;
    }

    /**
     * XToMany field and the related filterUnits.
     */
    @Data
    private static class XToManyFieldFilters {
        // The table alice of the model that the XToMany field belongs to
        private final String leftTableAlias;
        // XToMany field object
        private final MetaField metaField;
        // FilterUnit collection of the same XToMany field
        private final List<FilterUnit> filterUnits = new ArrayList<>();

        public XToManyFieldFilters(String tableAlias, MetaField metaField, FilterUnit filterUnit) {
            this.leftTableAlias = tableAlias;
            this.metaField = metaField;
            this.filterUnits.add(filterUnit);
        }

        public void addFilterUnit(FilterUnit filterUnit) {
            this.filterUnits.add(filterUnit);
        }
    }

    /**
     * Add the filter conditions to the same fieldAlias, which means queried based on the same XToMany field.
     *
     * @param tableAlias table alias of the model that the XToMany field belongs to
     * @param filterUnit filterUnit on the XToMany field
     * @param metaField XToMany field object
     */
    public void addXToManyFilterUnit(String tableAlias, MetaField metaField, FilterUnit filterUnit) {
        String fieldAlias = tableAlias + "." + metaField.getFieldName();
        if (this.xToManyFieldFiltersMap.containsKey(fieldAlias)) {
            this.xToManyFieldFiltersMap.get(fieldAlias).addFilterUnit(filterUnit);
        } else {
            this.xToManyFieldFiltersMap.put(fieldAlias, new XToManyFieldFilters(tableAlias, metaField, filterUnit));
        }
    }

    public boolean isEmpty() {
        return xToManyFieldFiltersMap.isEmpty();
    }

    public StringBuilder parse() {
        if (LogicOperator.OR.equals(logicOperator)) {
            return processOrLogical();
        } else {
            return processAndLogical();
        }
    }

    /**
     * Process the same level xToManyFieldFilters of OR logic.
     * Merge multiple filterUnits based on the same XToMany field.
     *
     * @return SQL fragment
     */
    private StringBuilder processOrLogical() {
        StringBuilder sb = new StringBuilder();
        for (XToManyFieldFilters xToManyFieldFilters : xToManyFieldFiltersMap.values()) {
            Filters xToManyFilters = new Filters();
            xToManyFieldFilters.getFilterUnits().forEach(filterUnit -> xToManyFilters.or(Filters.of(filterUnit)));
            // Get the ids of the main model through the XToMany field, according to the merged Filters conditions.
            // The operator after the value replacement is always `IN`.
            List<Serializable> ids = this.getIdsFromXToMany(xToManyFieldFilters.getMetaField(), xToManyFilters);
            buildXToMany(sb, xToManyFieldFilters.getLeftTableAlias(), ids);
        }
        return sb;
    }

    /**
     * Process the same level xToManyFieldFilters of AND logic.
     * Merge multiple filterUnits based on the same XToMany field.
     *
     * @return SQL fragment
     */
    private StringBuilder processAndLogical() {
        StringBuilder sb = new StringBuilder();
        for (XToManyFieldFilters xToManyFieldFilters : xToManyFieldFiltersMap.values()) {
            // Get the ids of the main model through the XToMany field. The operator after the value replacement is always `IN`.
            // The `negative filter` of the AND logic needs to do a reverse query.
            List<Serializable> mainIds = this.processNegativeSearch(xToManyFieldFilters.getMetaField(), xToManyFieldFilters.getFilterUnits());
            buildXToMany(sb, xToManyFieldFilters.getLeftTableAlias(), mainIds);
        }
        return sb;
    }

    /**
     * Build the SQL fragment of a single XToMany field, `tx.id IN mainIds`.
     *
     * @param sb         SQL fragment that has been built
     * @param tableAlias table alias of the model that the XToMany field belongs to
     * @param mainIds    ids of the main model that matches the conditions of the reverse query
     */
    private void buildXToMany(StringBuilder sb, String tableAlias, List<Serializable> mainIds) {
        if (CollectionUtils.isEmpty(mainIds)) {
            // When the mainIds is empty, the condition is always `FALSE`.
            if (!sb.isEmpty()) {
                sb.append(" ").append(logicOperator.name()).append(" ");
            }
            sb.append("FALSE");
        } else {
            MetaField idMetaField = ModelManager.getModelField(mainModel, ModelConstant.ID);
            String columnAlias = tableAlias + "." + ModelConstant.ID;
            FilterUnit idFilterUnit = new FilterUnit(columnAlias, Operator.IN, mainIds);
            StringBuilder xToMSql =  FilterUnitParser.parse(sqlWrapper, tableAlias, idMetaField, idFilterUnit);
            if (!sb.isEmpty() && !xToMSql.isEmpty()) {
                sb.append(" ").append(logicOperator.name()).append(" ");
            }
            sb.append(xToMSql);
        }
    }

    /**
     * Get the ids of the main model through the XToMany field.
     *
     * @param metaField XToMany field object
     * @param filters   filters based on the XToMany field
     * @return ids of the main model
     */
    private List<Serializable> getIdsFromXToMany(MetaField metaField, Filters filters) {
        List<Serializable> mainIds = new ArrayList<>();
        if (FieldType.ONE_TO_MANY.equals(metaField.getFieldType())) {
            // OneToMany scenario: directly query the main model ids
            mainIds = ReflectTool.getRelatedIds(metaField.getRelatedModel(), metaField.getRelatedField(), filters);
        } else {
            // ManyToMany scenario: query the ids of the related model, that is,
            // the values of the jointRight in the joint table.
            List<Serializable> rightIds = ReflectTool.getIds(metaField.getRelatedModel(), filters);
            // Query the joint table to get the main model ids
            if (!CollectionUtils.isEmpty(rightIds)) {
                Filters jointFilters = new Filters().in(metaField.getJointRight(), rightIds);
                mainIds = ReflectTool.getRelatedIds(metaField.getJointModel(), metaField.getJointLeft(), jointFilters);
            }
        }
        return mainIds;
    }

    /**
     * In the AND search mode of the XToMany field, when there is a negative search, such as `!=`, `NOT CONTAINS`, `NOT IN`,
     * all are converted to positive conditions, and searched by exclusion.
     *
     * @param metaField    XToMany field object
     * @param filterUnits  filterUnits based on the same XToMany field
     * @return ids after convert negative search to positive search
     */
    private List<Serializable> processNegativeSearch(MetaField metaField, List<FilterUnit> filterUnits) {
        boolean needReverse = false;
        Filters reversedFilters = new Filters();
        Filters positiveFilters = new Filters();
        for (FilterUnit filterUnit : filterUnits) {
            if (Operator.TO_MANY_NEGATIVE_OPERATORS.contains(filterUnit.getOperator())) {
                needReverse = true;
                // When reverse search is needed, reverse the filterUnit operator and connect them by OR.
                reversedFilters.or(filterUnit.getField(), filterUnit.getOperator().reverse(), filterUnit.getValue());
            } else {
                positiveFilters.and(filterUnit.getField(), filterUnit.getOperator(), filterUnit.getValue());
            }
        }
        List<Serializable> finalIds;
        if (needReverse) {
            finalIds = this.reverseSearch(metaField, positiveFilters, reversedFilters);
        } else {
            // When there is no negative filterUnit, directly query the main model ids.
            finalIds = this.getIdsFromXToMany(metaField, positiveFilters);
        }
        return finalIds;
    }

    /**
     * Reverse search XToMany field.
     *
     * @param metaField        field object
     * @param positiveFilters  positiveFilters
     * @param reversedFilters  reversedFilters
     * @return ids obtained by reverse search
     */
    private List<Serializable> reverseSearch(MetaField metaField, Filters positiveFilters, Filters reversedFilters) {
        List<Serializable> mainIds = new ArrayList<>();
        // Merge the positiveFilters and reversedFilters to generate the filters of the data to be excluded.
        Filters negationFilters = Filters.and(positiveFilters, reversedFilters);
        List<Serializable> excludedMainIds;
        if (FieldType.ONE_TO_MANY.equals(metaField.getFieldType())) {
            // OneToMany scenario
            excludedMainIds = ReflectTool.getRelatedIds(metaField.getRelatedModel(), metaField.getRelatedField(), negationFilters);
            // When the reverse search result is empty, there is no data to be excluded,
            // directly execute the positive search, otherwise merge and search.
            Filters finalJointFilters = CollectionUtils.isEmpty(excludedMainIds) ? positiveFilters : Filters.and(positiveFilters, Filters.of(metaField.getRelatedField(), Operator.NOT_IN, excludedMainIds));
            mainIds = ReflectTool.getRelatedIds(metaField.getRelatedModel(), metaField.getRelatedField(), finalJointFilters);
        } else {
            // ManyToMany scenario:
            // query the ids of the related model, that is, the values of the jointLeft in the joint model.
            List<Serializable> negativeRightIds = ReflectTool.getIds(metaField.getRelatedModel(), negationFilters);
            // Query the joint model to get the main model ids
            if (!CollectionUtils.isEmpty(negativeRightIds)) {
                Filters jointFilters = new Filters().in(metaField.getJointRight(), negativeRightIds);
                excludedMainIds = ReflectTool.getRelatedIds(metaField.getJointModel(), metaField.getJointLeft(), jointFilters);
                if (!CollectionUtils.isEmpty(excludedMainIds)) {
                    List<Serializable> positiveRightIds = ReflectTool.getIds(metaField.getRelatedModel(), positiveFilters);
                    if (!CollectionUtils.isEmpty(positiveRightIds)) {
                        Filters finalJointFilters = new Filters().notIn(metaField.getJointLeft(), excludedMainIds).in(metaField.getJointRight(), positiveRightIds);
                        mainIds = ReflectTool.getRelatedIds(metaField.getJointModel(), metaField.getJointLeft(), finalJointFilters);
                    }
                } else {
                    // When the reverse search result is empty, there is no data to be excluded,
                    mainIds = this.getIdsFromXToMany(metaField, positiveFilters);
                }
            }
        }
        return mainIds;
    }

}
