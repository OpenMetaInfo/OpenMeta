package info.openmeta.starter.flow.action.processors;

import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.GenerateReportParams;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import org.springframework.stereotype.Component;

/**
 * Processor for GenerateReport action.
 * Generate the report and put the result into the action context.
 */
@Component
public class GenerateReportAction implements ActionProcessor<GenerateReportParams> {

    @Override
    public FlowActionType getActionType() {
        return FlowActionType.GENERATE_REPORT;
    }

    @Override
    public Class<GenerateReportParams> getParamsClass() {
        return GenerateReportParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, GenerateReportParams actionParams) {
    }

    /**
     * Execute the GenerateReportAction processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     * @param actionContext The action context
     */
    @Override
    public void execute(FlowAction flowAction, GenerateReportParams actionParams, ActionContext actionContext) {
        // TODO
    }
}
