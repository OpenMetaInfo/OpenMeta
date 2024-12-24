package info.openmeta.starter.flow.node;

import info.openmeta.framework.base.exception.FlowException;
import info.openmeta.framework.base.exception.JSONException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.starter.flow.node.params.NodeParams;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.FlowNodeType;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Flow node factory
 */
@Slf4j
@Component
public class NodeFactory<T extends NodeParams> {

    private final Map<FlowNodeType, NodeProcessor<T>> NODE_PROCESSOR_MAP = new HashMap<>();

    /**
     * Inject all actions through the constructor
     *
     * @param nodeProcessors Spring automatic assembly collection,
     *                         automatically collect all implementations of NodeProcessor<T>
     */
    @Autowired
    public NodeFactory(List<NodeProcessor<T>> nodeProcessors) {
        for (NodeProcessor<T> nodeProcessor : nodeProcessors) {
            NODE_PROCESSOR_MAP.put(nodeProcessor.getNodeType(), nodeProcessor);
        }
    }

    /**
     * Execute the flow node processor
     *
     * @param flowNode    Flow node
     * @param nodeContext Environment variables for executing flow actions,
     *                      including row data that trigger the flow
     */
    public void executeNodeProcessor(FlowNode flowNode, NodeContext nodeContext) {
        // Get the node processor by node type
        NodeProcessor<T> nodeProcessor = NODE_PROCESSOR_MAP.get(flowNode.getNodeType());
        Assert.notNull(nodeProcessor, "The processor for flow node {0} does not exist!", flowNode.getNodeType());
        // Get the node parameter, and convert the node parameters to the corresponding parameter type instance
        Class<T> paramsClass = nodeProcessor.getParamsClass();
        T nodeParams;
        try {
            nodeParams = JsonMapper.jsonNodeToObject(flowNode.getNodeParams(), paramsClass);
        } catch (JSONException e) {
            throw new JSONException("Failed to convert the parameters of flow node {0} to an instance of {1}: {2}",
                    flowNode, paramsClass.getSimpleName(), e.getMessage());
        }
        Assert.notNull(nodeParams, "The parameters for flow node {0} cannot be null", flowNode);
        nodeProcessor.validateParams(flowNode, nodeParams);
        try {
            nodeProcessor.execute(flowNode, nodeParams, nodeContext);
        } catch (DuplicateKeyException e) {
            throw new FlowException("Failed to execute flow node due to unique index constraint violation: {0} ",
                    flowNode, e);
        } catch (Exception e) {
            throw new FlowException("Failed to execute flow node {0}! \n NodeParams: {} \n NodeContext: {}",
                    flowNode, nodeParams, nodeContext, e);
        }
        // Resolve and handle the exception result of FlowNode
        NodeExceptionResolver.resolveNodeException(flowNode, nodeContext);
    }

}
