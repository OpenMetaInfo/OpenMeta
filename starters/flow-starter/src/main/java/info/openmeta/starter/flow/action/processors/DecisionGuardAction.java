package info.openmeta.starter.flow.action.processors;

import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionExceptionResolver;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.DecisionGuardParams;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.ActionExceptionSignal;
import info.openmeta.starter.flow.enums.FlowActionType;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Processor for DecisionGuard action.
 * Control the flow direction based on the specified condition, such as exiting the flow, exiting the node, etc.
 */
@Slf4j
@Component
public class DecisionGuardAction implements ActionProcessor<DecisionGuardParams> {

    /**
     * Get the FlowActionType processed by the current processor.
     *
     * @return The FlowActionType associated with the current processor.
     */
    @Override
    public FlowActionType getActionType() {
        return FlowActionType.DECISION_GUARD;
    }

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    @Override
    public Class<DecisionGuardParams> getParamsClass() {
        return DecisionGuardParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, DecisionGuardParams actionParams) {
        Assert.notBlank(actionParams.getPassCondition(),
                "The pass condition expression for Decision Guard {0} cannot be blank!", flowAction.getName());
        Assert.notNull(actionParams.getExceptionSignal(),
                "The exception handling signal for Decision Guard {0} cannot be null!", flowAction.getName());
        if (ActionExceptionSignal.THROW_EXCEPTION.equals(actionParams.getExceptionSignal())) {
            Assert.notBlank(actionParams.getExceptionMessage(),
                    "Since Decision Guard {0} is configured to throw an exception, the exception message cannot be empty!",
                    flowAction.getName());
        }
    }

    /**
     * Execute the DecisionGuardAction processor.
     * Determines if the flow can continue based on the condition.
     * If true, the flow continues; if false, an exception signal is thrown.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     * @param actionContext The action context
     */
    @Override
    public void execute(FlowAction flowAction, DecisionGuardParams actionParams, ActionContext actionContext) {
        boolean passable = ComputeUtils.executeBoolean(actionParams.getPassCondition(), actionContext.getEnv());
        if (!passable) {
            ActionExceptionSignal exceptionSignal = actionParams.getExceptionSignal();
            // Interpolation calculation of the exception message
            String exceptionMessage = ComputeUtils.stringInterpolation(actionParams.getExceptionMessage(), actionContext.getEnv());
            // Process exception signal
            ActionExceptionResolver.processExceptionSignal(flowAction, exceptionSignal, exceptionMessage, actionContext);
        }
    }
}
