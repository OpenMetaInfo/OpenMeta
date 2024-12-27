package info.openmeta.starter.flow.utils;

import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.framework.orm.domain.FilterUnit;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.enums.FilterType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.entity.FlowNode;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import javax.annotation.Nullable;
import java.util.*;

/**
 * Flow utility class.
 * Providing methods for extracting variables, executing expressions, and resolving data templates.
 */
@Slf4j
public class FlowUtils {

    /**
     * Get the primary key list from the context based on the primary key variable.
     *
     * @param flowNode Flow node
     * @param pkVariable Primary key variable
     * @param nodeContext Node context
     * @return ids Primary key list
     */
    public static Collection<?> getIdsFromPkVariable(FlowNode flowNode, String pkVariable, NodeContext nodeContext) {
        // String variable parameter `#{}` is obtained from the environment variables.
        String variableName = pkVariable.substring(2, pkVariable.length() - 1);
        Assert.isTrue(nodeContext.containsKey(variableName),
                "In flow node {0}, the primary key parameter {1} does not exist in the nodeContext.",
                flowNode.getName(), variableName);
        Object pks = nodeContext.get(variableName);
        if (pks == null) {
            return Collections.emptyList();
        } else if (pks instanceof Collection) {
            return (Collection<?>) pks;
        } else {
            return Collections.singleton(pks);
        }
    }

    /**
     * Resolve a model-independent data Map based on the data template,
     * where the field value supports constants, variables, and calculation formulas.
     *
     * @param dataTemplate Data template
     * @param nodeContext Node context
     * @return New or updated data record
     */
    public static Map<String, Object> resolveDataTemplate(Map<String, Object> dataTemplate, NodeContext nodeContext) {
        return resolveRowTemplate(null, dataTemplate, nodeContext);
    }

    /**
     * Generate a new or updated data record based on the model row data template,
     * where the field value supports constants, variables, and calculation formulas.
     *
     * @param modelName Model name of the data to be operated, when it is empty,
     *                  the result of the calculation formula is not converted.
     * @param rowTemplate Model row data template
     * @param nodeContext Node context
     * @return New or updated model data
     */
    public static Map<String, Object> resolveRowTemplate(@Nullable String modelName, Map<String, Object> rowTemplate,
                                                         NodeContext nodeContext) {
        Map<String, Object> rowMap = new HashMap<>();
        rowTemplate.forEach((field, value) -> {
            if (value instanceof String && StringTools.isVariable((String) value)) {
                // Extract variable parameters `#{}` from the environment variables.
                Object fieldValue = StringTools.extractVariable((String) value, nodeContext.getEnv());
                rowMap.put(field, fieldValue);
            } else if (value instanceof String && StringTools.isExpression((String) value)) {
                // When the field value is a formula `${}`, the calculation result is converted to the actual value.
                Object result = executeExpression(modelName, field, (String) value, nodeContext);
                rowMap.put(field, result);
            } else {
                // When the value is a constant, directly assign the value to the field.
                rowMap.put(field, value);
            }
        });
        return rowMap;
    }

    /**
     * Execute the calculation expression `${}`.
     * The variables in the calculation expression must all be in the node context variables.
     *
     * @param modelName Model name of the data to be operated, when it is empty,
     *                  the result of the calculation formula is not converted.
     * @param field Field name
     * @param expressionLabel Calculation expression label
     * @param nodeContext Node context
     * @return Calculation expression result
     */
    public static Object executeExpression(@Nullable String modelName, String field, String expressionLabel,
                                           NodeContext nodeContext) {
        String expression = expressionLabel.substring(2, expressionLabel.length() - 1);
        // Determine if the variables in the calculation formula exist in the node context.
        List<String> dependentVariables = ComputeUtils.compile(expression).getVariableFullNames();
        dependentVariables.removeAll(nodeContext.keySet());
        Assert.isTrue(dependentVariables.isEmpty(), """
                        The variables {1} appear in the calculated expression for the data template parameter field {0},
                        do not exist in the node context.""", dependentVariables);
        // When modelName is empty, the result of the calculation formula is not converted.
        if (StringUtils.isBlank(modelName)) {
            return ComputeUtils.execute(expression, nodeContext.getEnv());
        } else {
            MetaField metaField = ModelManager.getModelField(modelName, field);
            return ComputeUtils.execute(expression, nodeContext.getEnv(), metaField.getScale(), metaField.getFieldType());
        }
    }

    /**
     * Convert variables in Filters to actual values, `#{}`.
     *
     * @param modelName Model name of the current node parameter
     * @param filters Filters
     * @param nodeContext Node context
     */
    public static void resolveFilterValue(String modelName, Filters filters, NodeContext nodeContext) {
        if (Filters.isEmpty(filters)) {
            return;
        }
        if (FilterType.LEAF.equals(filters.getType())
                && filters.getFilterUnit() != null
                && filters.getFilterUnit().getValue() instanceof String) {
            FilterUnit filterUnit = filters.getFilterUnit();
            String paramValue = (String) filterUnit.getValue();
            if (StringTools.isVariable(paramValue)) {
                // Extract variable parameter values `#{}`.
                Object value = StringTools.extractVariable(paramValue, nodeContext.getEnv());
                filterUnit.setValue(value);
                validateFilterUnitValue(filterUnit, paramValue);
            } else if (StringTools.isExpression(paramValue)) {
                // Execute the calculation expression `${}`, where the field in FilterUnit allows cascaded definition,
                // and the type of the last field is used as the actual assignment type.
                MetaField lastField = ModelManager.getLastFieldOfCascaded(modelName, filterUnit.getField());
                Object value = executeExpression(modelName, lastField.getFieldName(), paramValue, nodeContext);
                filterUnit.setValue(value);
                validateFilterUnitValue(filterUnit, paramValue);
            }
        } else if (FilterType.TREE.equals(filters.getType()) && filters.getChildren() != null) {
            List<Filters> children = filters.getChildren();
            children.forEach(child -> resolveFilterValue(modelName, child, nodeContext));
        }
    }

    /**
     * Validate the legality of the value in filterUnit.
     *
     * @param filterUnit FilterUnit object
     * @param paramValue Filter parameter value
     */
    private static void validateFilterUnitValue(FilterUnit filterUnit, String paramValue) {
        try {
            FilterUnit.validateFilterUnit(filterUnit);
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("Validation failed for the parameter {0} of filter condition {1}: {2}",
                    paramValue, filterUnit.toString(), e.getMessage());
        }
    }

}
