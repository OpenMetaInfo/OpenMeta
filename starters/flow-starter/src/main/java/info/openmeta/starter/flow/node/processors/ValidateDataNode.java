package info.openmeta.starter.flow.node.processors;

import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.node.NodeProcessor;
import info.openmeta.starter.flow.node.params.ValidateDataParams;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.FlowNodeType;
import org.springframework.stereotype.Component;

/**
 * Processor for ValidateData node.
 * Validate the data with the specified expression.
 */
@Component
public class ValidateDataNode implements NodeProcessor<ValidateDataParams> {

    @Override
    public FlowNodeType getNodeType() {
        return FlowNodeType.VALIDATE_DATA;
    }

    @Override
    public Class<ValidateDataParams> getParamsClass() {
        return ValidateDataParams.class;
    }

    /**
     * Validate the parameters of the specified FlowNode under the current node processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     */
    @Override
    public void validateParams(FlowNode flowNode, ValidateDataParams nodeParams) {
        Assert.notBlank(nodeParams.getExpression(),
                "The calculation formula for ValidateDataNode {0} cannot be empty!", flowNode.getName());
        Assert.notBlank(nodeParams.getExceptionMsg(),
                "The exception message for ValidateDataNode {0} cannot be empty!", flowNode.getName());
    }

    /**
     * Execute the ValidateDataNode processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     * @param nodeContext The node context
     */
    @Override
    public void execute(FlowNode flowNode, ValidateDataParams nodeParams, NodeContext nodeContext) {
        boolean isTrue = ComputeUtils.executeBoolean(nodeParams.getExpression(), nodeContext.getEnv());
        if (!isTrue) {
            // The exception message supports string interpolation and can use variables from the nodeContext.
            String exceptionMessage = ComputeUtils.stringInterpolation(nodeParams.getExceptionMsg(), nodeContext.getEnv());
            throw new BusinessException(exceptionMessage);
        }
    }
}
