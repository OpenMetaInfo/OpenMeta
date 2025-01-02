package info.openmeta.starter.flow.node.processors;

import info.openmeta.starter.ai.entity.AiMessage;
import info.openmeta.starter.ai.service.AiRobotService;
import info.openmeta.starter.ai.vo.ChatMessage;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.node.NodeProcessor;
import info.openmeta.starter.flow.node.params.QueryAiParams;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.FlowNodeType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Processor for QueryAi node.
 * Query AI and put the result into the node context.
 */
@Component
public class QueryAiNode implements NodeProcessor<QueryAiParams> {

    @Autowired
    private AiRobotService robotService;

    @Override
    public FlowNodeType getNodeType() {
        return FlowNodeType.QUERY_AI;
    }

    @Override
    public Class<QueryAiParams> getParamsClass() {
        return QueryAiParams.class;
    }

    /**
     * Validate the parameters of the specified FlowNode under the current node processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     */
    @Override
    public void validateParams(FlowNode flowNode, QueryAiParams nodeParams) {
        Assert.notBlank(nodeParams.getQueryContent(),
                "The query parameter for Query AI Node {0} cannot be empty.", flowNode.getName());
    }

    /**
     * Execute the QueryAiNode processor.
     * Query content supports string interpolation `#{var}`.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     * @param nodeContext The node context
     */
    @Override
    public void execute(FlowNode flowNode, QueryAiParams nodeParams, NodeContext nodeContext) {
        // Compute string interpolation
        String query = ComputeUtils.stringInterpolation(nodeParams.getQueryContent(), nodeContext.getEnv());
        ChatMessage chatMessage = new ChatMessage(nodeParams.getRobotId(), nodeParams.getConversationId(), query);
        AiMessage aiMessage = robotService.chat(chatMessage);
        // Put the AI reply content into the Node context
        nodeContext.put(flowNode.getId(), aiMessage.getAnswer());
    }
}
