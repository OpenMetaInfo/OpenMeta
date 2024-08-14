package info.openmeta.starter.flow.action.processors;

import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.TransferStageParams;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import org.springframework.stereotype.Component;

/**
 * Processor for TransferStage action.
 * Transfer the stage to the specified recipient.
 */
@Component
public class TransferStageAction implements ActionProcessor<TransferStageParams> {

    @Override
    public FlowActionType getActionType() {
        return FlowActionType.TRANSFER_STAGE;
    }

    @Override
    public Class<TransferStageParams> getParamsClass() {
        return TransferStageParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, TransferStageParams actionParams) {
    }

    /**
     * Execute the TransferStageAction processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     * @param actionContext The action context
     */
    @Override
    public void execute(FlowAction flowAction, TransferStageParams actionParams, ActionContext actionContext) {
        // TODO
    }
}
