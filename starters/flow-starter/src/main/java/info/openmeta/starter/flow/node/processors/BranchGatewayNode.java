package info.openmeta.starter.flow.node.processors;

import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.FlowNodeType;
import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.node.NodeProcessor;
import info.openmeta.starter.flow.node.params.BranchGatewayParams;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Processor for BranchGateway node.
 */
@Slf4j
@Component
public class BranchGatewayNode implements NodeProcessor<BranchGatewayParams> {

    /**
     * Get the FlowNodeType processed by the current processor.
     *
     * @return The FlowNodeType associated with the current processor.
     */
    @Override
    public FlowNodeType getNodeType() {
        return FlowNodeType.BRANCH_GATEWAY;
    }

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    @Override
    public Class<BranchGatewayParams> getParamsClass() {
        return BranchGatewayParams.class;
    }

    /**
     * Validate the parameters of the specified FlowNode under the current node processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     */
    @Override
    public void validateParams(FlowNode flowNode, BranchGatewayParams nodeParams) {
    }

    /**
     * Execute the BranchGatewayNode processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     * @param nodeContext The node context
     */
    @Override
    public void execute(FlowNode flowNode, BranchGatewayParams nodeParams, NodeContext nodeContext) {
    }
}
