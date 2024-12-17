package info.openmeta.starter.flow.action.processors;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.AsyncTaskParams;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import info.openmeta.starter.flow.message.FlowAsyncTaskProducer;
import info.openmeta.starter.flow.message.dto.FlowAsyncTaskMessage;
import info.openmeta.starter.flow.utils.FlowUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;

/**
 * Processor for AsyncTask action, which sends an asynchronous task message.
 */
@Slf4j
@Component
public class AsyncTaskAction implements ActionProcessor<AsyncTaskParams> {

    @Autowired
    private FlowAsyncTaskProducer flowAsyncTaskProducer;

    /**
     * Get the FlowActionType processed by the current processor.
     *
     * @return The FlowActionType associated with the current processor.
     */
    @Override
    public FlowActionType getActionType() {
        return FlowActionType.ASYNC_TASK;
    }

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    @Override
    public Class<AsyncTaskParams> getParamsClass() {
        return AsyncTaskParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, AsyncTaskParams actionParams) {
        Assert.notBlank(actionParams.getAsyncTaskHandlerCode(),
                "The async task handler code {0} cannot be empty!",
                flowAction.getName());
    }

    /**
     * Execute the AsyncTaskAction processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     * @param actionContext The action context
     */
    @Override
    public void execute(FlowAction flowAction, AsyncTaskParams actionParams, ActionContext actionContext) {
        // Resolve the asynchronous task parameter data template
        Map<String, Object> asyncTaskParams = FlowUtils.resolveDataTemplate(actionParams.getDataTemplate(), actionContext);
        // Construct an asynchronous task message
        FlowAsyncTaskMessage flowAsyncTaskMessage = new FlowAsyncTaskMessage();
        flowAsyncTaskMessage.setFlowId(flowAction.getFlowId());
        flowAsyncTaskMessage.setNodeId(flowAction.getNodeId());
        flowAsyncTaskMessage.setActionId(flowAction.getId());
        flowAsyncTaskMessage.setAsyncTaskHandlerCode(actionParams.getAsyncTaskHandlerCode());
        flowAsyncTaskMessage.setAsyncTaskParams(asyncTaskParams);
        flowAsyncTaskMessage.setContext(ContextHolder.getContext());
        // Send the asynchronous task message
        flowAsyncTaskProducer.sendFlowTask(flowAsyncTaskMessage);
    }
}
