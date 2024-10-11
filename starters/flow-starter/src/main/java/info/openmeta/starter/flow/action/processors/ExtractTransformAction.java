package info.openmeta.starter.flow.action.processors;

import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.ExtractTransformParams;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.*;

/**
 * Processor for ExtractTransform action.
 * Extract and transform data from the specified collection variable.
 */
@Slf4j
@Component
public class ExtractTransformAction implements ActionProcessor<ExtractTransformParams> {

    /**
     * Get the FlowActionType processed by the current processor.
     *
     * @return The FlowActionType associated with the current processor.
     */
    @Override
    public FlowActionType getActionType() {
        return FlowActionType.EXTRACT_TRANSFORM;
    }

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    @Override
    public Class<ExtractTransformParams> getParamsClass() {
        return ExtractTransformParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, ExtractTransformParams actionParams) {
        Assert.notBlank(actionParams.getCollectionVariable(),
                "The collection parameter configuration for Extract-Transform Action {0} cannot be empty!",
                flowAction.getName());
        Assert.isTrue(StringTools.isVariable(actionParams.getCollectionVariable()),
                "The parameter {0} for Extract-Transform Action {1} must be identified with `#{}`.",
                actionParams.getCollectionVariable(), flowAction.getName());
        Assert.notBlank(actionParams.getItemKey(),
                "The item key configuration for Extract-Transform Action {0} cannot be empty!",
                flowAction.getName());
    }

    /**
     * Execute the ExtractTransformAction processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     * @param actionContext The action context
     */
    @Override
    public void execute(FlowAction flowAction, ExtractTransformParams actionParams, ActionContext actionContext) {
        Object variableValue = StringTools.extractVariable(actionParams.getCollectionVariable(), actionContext.getEnv());
        if (variableValue == null || (variableValue instanceof Collection && CollectionUtils.isEmpty((Collection<?>) variableValue))) {
            actionContext.put(flowAction.getCode(), Collections.emptySet());
        } else if (variableValue instanceof Collection) {
            Set<Object> result = new HashSet<>();
            ((Collection<?>) variableValue).forEach(row -> {
                if (row instanceof Map) {
                    Object val = ((Map<?, ?>) row).get(actionParams.getItemKey());
                    if (val != null) {
                        result.add(val);
                    }
                }
            });
            actionContext.put(flowAction.getCode(), result);
        } else {
            throw new IllegalArgumentException("""
                    The value of the data source variable {0} for Extract-Transform Action {1} is not a collection: {2}.
                    """, actionParams.getCollectionVariable(), flowAction.getName(), variableValue);
        }
    }
}
