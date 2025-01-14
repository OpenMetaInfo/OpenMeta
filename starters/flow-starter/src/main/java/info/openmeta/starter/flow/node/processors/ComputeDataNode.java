package info.openmeta.starter.flow.node.processors;

import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.node.NodeProcessor;
import info.openmeta.starter.flow.node.params.ComputeDataParams;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.FlowNodeType;
import org.springframework.stereotype.Component;

/**
 * Processor for ComputeData node.
 * Calculating the data and putting the result into the node context.
 */
@Component
public class ComputeDataNode implements NodeProcessor<ComputeDataParams> {

    /**
     * Get the FlowNodeType processed by the current processor.
     *
     * @return The FlowNodeType associated with the current processor.
     */
    @Override
    public FlowNodeType getNodeType() {
        return FlowNodeType.COMPUTE_DATA;
    }

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    @Override
    public Class<ComputeDataParams> getParamsClass() {
        return ComputeDataParams.class;
    }

    /**
     * Validate the parameters of the specified FlowNode under the current node processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     */
    @Override
    public void validateParams(FlowNode flowNode, ComputeDataParams nodeParams) {
        Assert.notBlank(nodeParams.getExpression(),
                "The calculation formula for the ComputeDataNode {0} cannot be empty.", flowNode.getName());
    }

    /**
     * Execute the calculation node processor, and put the calculation result into the node context.
     *
     * @param flowNode Flow node
     * @param nodeParams ComputeDataParams object
     * @param nodeContext Node context
     */
    @Override
    public void execute(FlowNode flowNode, ComputeDataParams nodeParams, NodeContext nodeContext) {
        Object result = ComputeUtils.execute(nodeParams.getExpression(), nodeContext.getEnv(), nodeParams.getValueType());
        nodeContext.put(flowNode.getId(), result);
    }


}
