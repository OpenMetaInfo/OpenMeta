package info.openmeta.starter.flow.action;

import info.openmeta.starter.flow.action.params.ActionParams;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;

public interface ActionProcessor<T extends ActionParams> {

    /**
     * Get the FlowActionType processed by the current processor.
     *
     * @return The FlowActionType associated with the current processor.
     */
    FlowActionType getActionType();

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    Class<T> getParamsClass();

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    void validateParams(FlowAction flowAction, T actionParams);

    /**
     * Execute the current processor action.
     *
     * @param flowAction Flow action
     * @param actionParams Action parameters
     * @param actionContext Action context
     */
    void execute(FlowAction flowAction, T actionParams, ActionContext actionContext);
}
