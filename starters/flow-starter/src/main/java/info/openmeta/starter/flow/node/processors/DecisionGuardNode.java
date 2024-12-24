package info.openmeta.starter.flow.node.processors;

import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.node.NodeExceptionResolver;
import info.openmeta.starter.flow.node.NodeProcessor;
import info.openmeta.starter.flow.node.params.DecisionGuardParams;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.NodeExceptionSignal;
import info.openmeta.starter.flow.enums.FlowNodeType;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Processor for DecisionGuard node.
 * Control the flow direction based on the specified condition, such as exiting the flow, exiting the node, etc.
 */
@Slf4j
@Component
public class DecisionGuardNode implements NodeProcessor<DecisionGuardParams> {

    /**
     * Get the FlowNodeType processed by the current processor.
     *
     * @return The FlowNodeType associated with the current processor.
     */
    @Override
    public FlowNodeType getNodeType() {
        return FlowNodeType.DECISION_GUARD;
    }

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    @Override
    public Class<DecisionGuardParams> getParamsClass() {
        return DecisionGuardParams.class;
    }

    /**
     * Validate the parameters of the specified FlowNode under the current node processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     */
    @Override
    public void validateParams(FlowNode flowNode, DecisionGuardParams nodeParams) {
        Assert.notBlank(nodeParams.getPassCondition(),
                "The pass condition expression for Decision Guard {0} cannot be blank!", flowNode.getName());
        Assert.notNull(nodeParams.getExceptionSignal(),
                "The exception handling signal for Decision Guard {0} cannot be null!", flowNode.getName());
        if (NodeExceptionSignal.THROW_EXCEPTION.equals(nodeParams.getExceptionSignal())) {
            Assert.notBlank(nodeParams.getExceptionMessage(),
                    "Since Decision Guard {0} is configured to throw an exception, the exception message cannot be empty!",
                    flowNode.getName());
        }
    }

    /**
     * Execute the DecisionGuardNode processor.
     * Determines if the flow can continue based on the condition.
     * If true, the flow continues; if false, an exception signal is thrown.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     * @param nodeContext The node context
     */
    @Override
    public void execute(FlowNode flowNode, DecisionGuardParams nodeParams, NodeContext nodeContext) {
        boolean passable = ComputeUtils.executeBoolean(nodeParams.getPassCondition(), nodeContext.getEnv());
        if (!passable) {
            NodeExceptionSignal exceptionSignal = nodeParams.getExceptionSignal();
            // Interpolation calculation of the exception message
            String exceptionMessage = ComputeUtils.stringInterpolation(nodeParams.getExceptionMessage(), nodeContext.getEnv());
            // Process exception signal
            NodeExceptionResolver.processExceptionSignal(flowNode, exceptionSignal, exceptionMessage, nodeContext);
        }
    }
}
