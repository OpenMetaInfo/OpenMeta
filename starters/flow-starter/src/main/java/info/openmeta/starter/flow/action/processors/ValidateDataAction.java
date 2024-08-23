package info.openmeta.starter.flow.action.processors;

import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.ValidateDataParams;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import org.springframework.stereotype.Component;

/**
 * Processor for ValidateData action.
 * Validate the data with the specified expression.
 */
@Component
public class ValidateDataAction implements ActionProcessor<ValidateDataParams> {

    @Override
    public FlowActionType getActionType() {
        return FlowActionType.VALIDATE_DATA;
    }

    @Override
    public Class<ValidateDataParams> getParamsClass() {
        return ValidateDataParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, ValidateDataParams actionParams) {
        Assert.notBlank(actionParams.getExpression(),
                "The calculation formula for ValidateDataAction {0} cannot be empty!", flowAction.getName());
        Assert.notBlank(actionParams.getExceptionMsg(),
                "The exception message for ValidateDataAction {0} cannot be empty!", flowAction.getName());
    }

    /**
     * Execute the ValidateDataAction processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     * @param actionContext The action context
     */
    @Override
    public void execute(FlowAction flowAction, ValidateDataParams actionParams, ActionContext actionContext) {
        boolean isTrue = ComputeUtils.executeBoolean(actionParams.getExpression(), actionContext.getEnv());
        if (!isTrue) {
            // The exception message supports string interpolation and can use variables from the actionContext.
            String exceptionMessage = ComputeUtils.stringInterpolation(actionParams.getExceptionMsg(), actionContext.getEnv());
            throw new BusinessException(exceptionMessage);
        }
    }
}
