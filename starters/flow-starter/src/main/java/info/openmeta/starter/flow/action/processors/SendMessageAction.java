package info.openmeta.starter.flow.action.processors;

import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.SendMessageParams;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import org.springframework.stereotype.Component;

/**
 * Processor for SendMessage action.
 * Send the message to the specified recipient.
 */
@Component
public class SendMessageAction implements ActionProcessor<SendMessageParams> {

    @Override
    public FlowActionType getActionType() {
        return FlowActionType.SEND_MESSAGE;
    }

    @Override
    public Class<SendMessageParams> getParamsClass() {
        return SendMessageParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, SendMessageParams actionParams) {
    }

    /**
     * Execute the SendMessageAction processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     * @param actionContext The action context
     */
    @Override
    public void execute(FlowAction flowAction, SendMessageParams actionParams, ActionContext actionContext) {
        // TODO
    }
}
