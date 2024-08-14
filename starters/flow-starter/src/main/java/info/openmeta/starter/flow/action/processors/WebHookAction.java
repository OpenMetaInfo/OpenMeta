package info.openmeta.starter.flow.action.processors;

import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.WebHookParams;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import org.springframework.stereotype.Component;

/**
 * Processor for WebHook action.
 * Call the specified WebHook API.
 */
@Component
public class WebHookAction implements ActionProcessor<WebHookParams> {

    @Override
    public FlowActionType getActionType() {
        return FlowActionType.WEB_HOOK;
    }

    @Override
    public Class<WebHookParams> getParamsClass() {
        return WebHookParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, WebHookParams actionParams) {
    }

    /**
     * Execute the WebHookAction processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     * @param actionContext The action context
     */
    @Override
    public void execute(FlowAction flowAction, WebHookParams actionParams, ActionContext actionContext) {
        // TODO
    }
}
