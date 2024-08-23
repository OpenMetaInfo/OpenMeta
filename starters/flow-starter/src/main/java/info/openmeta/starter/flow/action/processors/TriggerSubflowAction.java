package info.openmeta.starter.flow.action.processors;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.starter.flow.FlowAutomation;
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.TriggerSubflowParams;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import info.openmeta.starter.flow.utils.FlowUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import java.util.Map;

/**
 * Processor for TriggerSubflow action.
 * Trigger a subflow by the specified subflow trigger model and code.
 */
@Slf4j
@Component
public class TriggerSubflowAction implements ActionProcessor<TriggerSubflowParams> {

    @Autowired
    @Lazy
    private FlowAutomation automation;

    /**
     * Get the FlowActionType processed by the current processor.
     *
     * @return The FlowActionType associated with the current processor.
     */
    @Override
    public FlowActionType getActionType() {
        return FlowActionType.TRIGGER_SUBFLOW;
    }

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    @Override
    public Class<TriggerSubflowParams> getParamsClass() {
        return TriggerSubflowParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, TriggerSubflowParams actionParams) {
        Assert.notBlank(actionParams.getSubflowTriggerModel(),
                "The subflow trigger model for Action {0} cannot be blank!", flowAction.getName());
        Assert.notBlank(actionParams.getSubflowTriggerCode(),
                "The subflow trigger code for Action {0} cannot be blank!", flowAction.getName());
    }

    /**
     * Execute the TriggerSubflowAction processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     * @param actionContext The action context
     */
    @Override
    public void execute(FlowAction flowAction, TriggerSubflowParams actionParams, ActionContext actionContext) {
        // Resolve the subflow parameter data template
        Map<String, Object> subflowParams = FlowUtils.resolveDataTemplate(actionParams.getDataTemplate(), actionContext);
        // Trigger the subflow through the subflow event and pass the subflow parameters
        Object result = automation.subflowEvent(actionParams.getSubflowTriggerModel(), actionParams.getSubflowTriggerCode(), subflowParams);
        // Put the subflow execution result into the Action context
        actionContext.put(flowAction.getCode(), result);
    }
}
