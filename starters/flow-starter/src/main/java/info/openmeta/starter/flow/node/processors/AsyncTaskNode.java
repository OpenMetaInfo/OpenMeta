package info.openmeta.starter.flow.node.processors;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.node.NodeProcessor;
import info.openmeta.starter.flow.node.params.AsyncTaskParams;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.FlowNodeType;
import info.openmeta.starter.flow.message.FlowAsyncTaskProducer;
import info.openmeta.starter.flow.message.dto.FlowAsyncTaskMessage;
import info.openmeta.starter.flow.utils.FlowUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;

/**
 * Processor for AsyncTask node, which sends an asynchronous task message.
 */
@Slf4j
@Component
public class AsyncTaskNode implements NodeProcessor<AsyncTaskParams> {

    @Autowired
    private FlowAsyncTaskProducer flowAsyncTaskProducer;

    /**
     * Get the FlowNodeType processed by the current processor.
     *
     * @return The FlowNodeType associated with the current processor.
     */
    @Override
    public FlowNodeType getNodeType() {
        return FlowNodeType.ASYNC_TASK;
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
     * Validate the parameters of the specified FlowNode under the current node processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     */
    @Override
    public void validateParams(FlowNode flowNode, AsyncTaskParams nodeParams) {
        Assert.notBlank(nodeParams.getAsyncTaskHandlerCode(),
                "The async task handler code {0} cannot be empty!",
                flowNode.getName());
    }

    /**
     * Execute the AsyncTaskNode processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     * @param nodeContext The node context
     */
    @Override
    public void execute(FlowNode flowNode, AsyncTaskParams nodeParams, NodeContext nodeContext) {
        // Resolve the asynchronous task parameter data template
        Map<String, Object> asyncTaskParams = FlowUtils.resolveDataTemplate(nodeParams.getDataTemplate(), nodeContext);
        // Construct an asynchronous task message
        FlowAsyncTaskMessage flowAsyncTaskMessage = new FlowAsyncTaskMessage();
        flowAsyncTaskMessage.setFlowId(flowNode.getFlowId());
        flowAsyncTaskMessage.setNodeId(flowNode.getId());
        flowAsyncTaskMessage.setAsyncTaskHandlerCode(nodeParams.getAsyncTaskHandlerCode());
        flowAsyncTaskMessage.setAsyncTaskParams(asyncTaskParams);
        flowAsyncTaskMessage.setContext(ContextHolder.getContext());
        // Send the asynchronous task message
        flowAsyncTaskProducer.sendFlowTask(flowAsyncTaskMessage);
    }
}
