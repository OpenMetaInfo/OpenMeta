package info.openmeta.starter.flow.action.processors;

import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.ComputeDataParams;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import org.springframework.stereotype.Component;

/**
 * Processor for ComputeData action.
 * Calculating the data and putting the result into the action context.
 */
@Component
public class ComputeDataAction implements ActionProcessor<ComputeDataParams> {

    /**
     * Get the FlowActionType processed by the current processor.
     *
     * @return The FlowActionType associated with the current processor.
     */
    @Override
    public FlowActionType getActionType() {
        return FlowActionType.COMPUTE_DATA;
    }

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    @Override
    public Class<ComputeDataParams> getParamsClass() {
        return ComputeDataParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, ComputeDataParams actionParams) {
        Assert.notBlank(actionParams.getExpression(),
                "The calculation formula for the ComputeDataAction {0} cannot be empty.", flowAction.getName());
    }

    /**
     * Execute the calculation action processor, and put the calculation result into the action context.
     *
     * @param flowAction Flow action
     * @param actionParams ComputeDataParams object
     * @param actionContext Action context
     */
    @Override
    public void execute(FlowAction flowAction, ComputeDataParams actionParams, ActionContext actionContext) {
        Object result = ComputeUtils.execute(actionParams.getExpression(), actionContext.getEnv(), actionParams.getValueType());
        actionContext.put(flowAction.getCode(), result);
    }
}
