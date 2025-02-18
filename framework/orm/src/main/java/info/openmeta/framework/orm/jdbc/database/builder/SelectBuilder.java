package info.openmeta.framework.orm.jdbc.database.builder;

import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.jdbc.database.SqlWrapper;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.base.utils.Assert;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.CollectionUtils;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Select Builder
 * Build select conditions for non-aggregate queries
 */
public class SelectBuilder extends BaseBuilder implements SqlClauseBuilder {

    public SelectBuilder(SqlWrapper sqlWrapper, FlexQuery flexQuery) {
        super(sqlWrapper, flexQuery);
    }

    public void build() {
        // Handle the select conditions for non-aggregate queries
        if (!flexQuery.isAggregate()) {
            // Process `distinct` before adding `SELECT` fields
            if (flexQuery.isDistinct()) {
                sqlWrapper.distinct();
            }
            handleSelect();
        }
    }

    /**
     * Construct the SELECT fields based on the field set of `flexQuery.getFields()`.
     * When the fields are not specified, use all fields of the main model by default,
     * and the security layer performs permission checks and exclusions.
     */
    private void handleSelect() {
        Set<String> selectFields;
        if (CollectionUtils.isEmpty(flexQuery.getFields())) {
            // When the fields are not specified, use all fields of the main model by default.
            selectFields = ModelManager.getModelFieldsWithoutXToMany(mainModelName);
        } else {
            selectFields = new HashSet<>(flexQuery.getFields());
            // Get the dependent fields of the dynamic calculation and cascaded fields in selectFields,
            // and add them to selectFields, which will be updated to flexQuery.fields later.
            Set<String> dependentFields = getSelectDependFields(selectFields);
            selectFields.addAll(dependentFields);
            // Add the ID field by default
            selectFields.add(ModelConstant.ID);
        }
        // When subQuery is specified, append the XToMany relationship fields in subQuery to selectFields.
        if (flexQuery.getSubQueries() != null) {
            selectFields.addAll(flexQuery.getSubQueries().getQueryMap().keySet());
        }
        // Update flexQuery.fields
        flexQuery.setFields(selectFields);
        // Only SELECT the stored fields of the current model, excluding cascaded fields (a.b.c), non-stored fields,
        // and the excluded fields will be filled in the return value formatting.
        List<String> storedFields = selectFields.stream()
                .filter(f -> ModelManager.existField(mainModelName, f) && ModelManager.isStored(mainModelName, f))
                .collect(Collectors.toList());
        sqlWrapper.select(this.parseStoredFields(storedFields, true));
        // When cross-timeline access, the main model is a timeline model, and the ManyToOne/OneToOne field results
        // need to be enhanced by join clause, add the displayNames of the associated timeline model to the SELECT fields.
        if (flexQuery.isAcrossTimeline()
                && ModelManager.isTimelineModel(mainModelName)
                && ConvertType.EXPAND_TYPES.contains(flexQuery.getConvertType())) {
            sqlWrapper.select(appendTimelineModelFields(storedFields));
        }
    }

    /**
     * Get the set of dependent fields of dynamic cascaded fields and computed fields.
     *
     * @param fieldNames field name set
     * @return set of dependent fields of dynamic cascaded and computed fields
     */
    private Set<String> getSelectDependFields(Set<String> fieldNames) {
        Set<String> dependentFields = new HashSet<>();
        fieldNames.forEach(field -> {
            if (field.contains(".")) {
                String xToOneField = StringUtils.split(field, ".")[0];
                Assert.isTrue(ModelManager.existField(mainModelName, xToOneField),
                        "Model {0} does not have relation field {1} in cascade field {2}.",
                        mainModelName, xToOneField, field);
                dependentFields.add(xToOneField);
            } else {
                MetaField metaField = ModelManager.getModelField(mainModelName, field);
                if (StringUtils.isNotBlank(metaField.getCascadedField()) && metaField.isDynamic()) {
                    // The dependent fields of dynamic cascaded fields
                    dependentFields.add(StringUtils.split(metaField.getCascadedField(), ".")[0]);
                } else if (metaField.isComputed() && metaField.isDynamic()) {
                    // The dependent fields of computed fields
                    dependentFields.addAll(metaField.getDependentFields());
                }
            }
        });
        return dependentFields;
    }

    /**
     * When cross-timeline access, both the main model and related model are timeline models,
     * and the ManyToOne/OneToOne field results need to be enhanced,
     * add the displayNames of the related model to the SELECT fields.
     * And add aliases to these SELECT fields, such as: t1.name AS deptId.name
     *
     * @param selectFields field set to be SELECT
     * @return field set of the related timeline model to be SELECT and their aliases
     */
    private Set<String> appendTimelineModelFields(Collection<String> selectFields) {
        Set<String> timelineModelFields = new HashSet<>();
        selectFields.forEach(field -> {
            MetaField metaField = ModelManager.getModelField(mainModelName, field);
            Set<String> cascadedFields = new HashSet<>();
            if (FieldType.TO_ONE_TYPES.contains(metaField.getFieldType())
                    && ModelManager.isTimelineModel(metaField.getRelatedModel())) {
                // ManyToOne/OneToOne fields, the associated model is a timeline model
                ModelManager.getModelDisplayName(metaField.getRelatedModel())
                        .forEach(displayField -> cascadedFields.add(field + "." + displayField));
            } else if (StringUtils.isNotBlank(metaField.getCascadedField()) && metaField.isDynamic()) {
                // Get dynamic cascaded fields
                String[] fieldArray = StringUtils.split(metaField.getCascadedField(), ".");
                MetaField casMetaField = ModelManager.getModelField(mainModelName, fieldArray[0]);
                if (ModelManager.isTimelineModel(casMetaField.getRelatedModel())) {
                    cascadedFields.add(metaField.getCascadedField());
                }
            }
            cascadedFields.forEach(cascadedField -> {
                String fieldAlias = parseLogicField(cascadedField, false) + " AS " + cascadedField;
                timelineModelFields.add(fieldAlias);
            });
        });
        return timelineModelFields;
    }

}
