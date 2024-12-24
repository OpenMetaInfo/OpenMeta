package info.openmeta.starter.flow.node.processors;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.starter.flow.FlowAutomation;
import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.node.NodeProcessor;
import info.openmeta.starter.flow.node.params.TriggerSubflowParams;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.FlowNodeType;
import info.openmeta.starter.flow.utils.FlowUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import java.util.Map;

/**
 * Processor for TriggerSubflow node.
 * Trigger a subflow by the specified subflow trigger model and code.
 */
@Slf4j
@Component
public class TriggerSubflowNode implements NodeProcessor<TriggerSubflowParams> {

    @Autowired
    @Lazy
    private FlowAutomation automation;

    /**
     * Get the FlowNodeType processed by the current processor.
     *
     * @return The FlowNodeType associated with the current processor.
     */
    @Override
    public FlowNodeType getNodeType() {
        return FlowNodeType.TRIGGER_SUBFLOW;
    }

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    @Override
    public Class<TriggerSubflowParams> getParamsClass() {
        return TriggerSubflowParams.class;
    }

    /**
     * Validate the parameters of the specified FlowNode under the current node processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     */
    @Override
    public void validateParams(FlowNode flowNode, TriggerSubflowParams nodeParams) {
        Assert.notBlank(nodeParams.getSubflowTriggerModel(),
                "The subflow trigger model for Node {0} cannot be blank!", flowNode.getName());
        Assert.notBlank(nodeParams.getSubflowTriggerCode(),
                "The subflow trigger code for Node {0} cannot be blank!", flowNode.getName());
    }

    /**
     * Execute the TriggerSubflowNode processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     * @param nodeContext The node context
     */
    @Override
    public void execute(FlowNode flowNode, TriggerSubflowParams nodeParams, NodeContext nodeContext) {
        // Resolve the subflow parameter data template
        Map<String, Object> subflowParams = FlowUtils.resolveDataTemplate(nodeParams.getDataTemplate(), nodeContext);
        // Trigger the subflow through the subflow event and pass the subflow parameters
        Object result = automation.subflowEvent(nodeParams.getSubflowTriggerModel(), nodeParams.getSubflowTriggerCode(), subflowParams);
        // Put the subflow execution result into the Node context
        nodeContext.put(flowNode.getCode(), result);
    }
}
