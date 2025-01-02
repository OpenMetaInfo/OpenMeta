package info.openmeta.starter.flow.node.processors;

import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.node.NodeProcessor;
import info.openmeta.starter.flow.node.params.WebHookParams;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.FlowNodeType;
import org.springframework.stereotype.Component;

/**
 * Processor for WebHook node.
 * Call the specified WebHook API.
 */
@Component
public class WebHookNode implements NodeProcessor<WebHookParams> {

    @Override
    public FlowNodeType getNodeType() {
        return FlowNodeType.WEB_HOOK;
    }

    @Override
    public Class<WebHookParams> getParamsClass() {
        return WebHookParams.class;
    }

    /**
     * Validate the parameters of the specified FlowNode under the current node processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     */
    @Override
    public void validateParams(FlowNode flowNode, WebHookParams nodeParams) {
    }

    /**
     * Execute the WebHookNode processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     * @param nodeContext The node context
     */
    @Override
    public void execute(FlowNode flowNode, WebHookParams nodeParams, NodeContext nodeContext) {
        // TODO
    }
}
