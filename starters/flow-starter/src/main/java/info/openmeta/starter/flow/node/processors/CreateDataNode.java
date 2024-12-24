package info.openmeta.starter.flow.node.processors;

import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.node.NodeProcessor;
import info.openmeta.starter.flow.node.params.CreateDataParams;
import info.openmeta.starter.flow.utils.FlowUtils;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.FlowNodeType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.Map;

/**
 * Processor for CreateData node.
 * Create data based on the specified data template.
 */
@Component
public class CreateDataNode implements NodeProcessor<CreateDataParams> {

    @Autowired
    private ModelService<? extends Serializable> modelService;

    /**
     * Get the FlowNodeType processed by the current processor.
     *
     * @return The FlowNodeType associated with the current processor.
     */
    @Override
    public FlowNodeType getNodeType() {
        return FlowNodeType.CREATE_DATA;
    }

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    @Override
    public Class<CreateDataParams> getParamsClass() {
        return CreateDataParams.class;
    }

    /**
     * Validate the parameters of the specified FlowNode under the current node processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     */
    @Override
    public void validateParams(FlowNode flowNode, CreateDataParams nodeParams) {
        Assert.notBlank(nodeParams.getModelName(),
                "The model name parameter for node {0} cannot be empty!", flowNode.getName());
        Assert.notEmpty(nodeParams.getRowTemplate(),
                "The data template parameter for node {0} cannot be empty!", flowNode.getName());
    }

    /**
     * Execute the CreateDataNode processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     * @param nodeContext The node context
     */
    @Override
    public void execute(FlowNode flowNode, CreateDataParams nodeParams, NodeContext nodeContext) {
        // Generate updated data based on the data template
        Map<String, Object> rowMap = FlowUtils.resolveRowTemplate(nodeParams.getModelName(), nodeParams.getRowTemplate(), nodeContext);
        modelService.createOne(nodeParams.getModelName(), rowMap);
    }
}
