package info.openmeta.starter.flow.node;

import info.openmeta.starter.flow.node.params.NodeParams;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.FlowNodeType;

public interface NodeProcessor<T extends NodeParams> {

    /**
     * Get the FlowNodeType processed by the current processor.
     *
     * @return The FlowNodeType associated with the current processor.
     */
    FlowNodeType getNodeType();

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    Class<T> getParamsClass();

    /**
     * Validate the parameters of the specified FlowNode under the current node processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     */
    void validateParams(FlowNode flowNode, T nodeParams);

    /**
     * Execute the current processor node.
     *
     * @param flowNode Flow node
     * @param nodeParams Node parameters
     * @param nodeContext Node context
     */
    void execute(FlowNode flowNode, T nodeParams, NodeContext nodeContext);
}
