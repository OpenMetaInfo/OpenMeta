package info.openmeta.starter.flow.node.processors;

import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.node.NodeProcessor;
import info.openmeta.starter.flow.node.params.ExtractTransformParams;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.FlowNodeType;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.*;

/**
 * Processor for ExtractTransform node.
 * Extract and transform data from the specified collection variable.
 */
@Slf4j
@Component
public class ExtractTransformNode implements NodeProcessor<ExtractTransformParams> {

    /**
     * Get the FlowNodeType processed by the current processor.
     *
     * @return The FlowNodeType associated with the current processor.
     */
    @Override
    public FlowNodeType getNodeType() {
        return FlowNodeType.EXTRACT_TRANSFORM;
    }

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    @Override
    public Class<ExtractTransformParams> getParamsClass() {
        return ExtractTransformParams.class;
    }

    /**
     * Validate the parameters of the specified FlowNode under the current node processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     */
    @Override
    public void validateParams(FlowNode flowNode, ExtractTransformParams nodeParams) {
        Assert.notBlank(nodeParams.getCollectionVariable(),
                "The collection parameter configuration for Extract-Transform Node {0} cannot be empty!",
                flowNode.getName());
        Assert.isTrue(StringTools.isVariable(nodeParams.getCollectionVariable()),
                "The parameter {0} for Extract-Transform Node {1} must be identified with `#{}`.",
                nodeParams.getCollectionVariable(), flowNode.getName());
        Assert.notBlank(nodeParams.getItemKey(),
                "The item key configuration for Extract-Transform Node {0} cannot be empty!",
                flowNode.getName());
    }

    /**
     * Execute the ExtractTransformNode processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     * @param nodeContext The node context
     */
    @Override
    public void execute(FlowNode flowNode, ExtractTransformParams nodeParams, NodeContext nodeContext) {
        Object variableValue = StringTools.extractVariable(nodeParams.getCollectionVariable(), nodeContext.getEnv());
        if (variableValue == null || (variableValue instanceof Collection && CollectionUtils.isEmpty((Collection<?>) variableValue))) {
            nodeContext.put(flowNode.getCode(), Collections.emptySet());
        } else if (variableValue instanceof Collection) {
            Set<Object> result = new HashSet<>();
            ((Collection<?>) variableValue).forEach(row -> {
                if (row instanceof Map) {
                    Object val = ((Map<?, ?>) row).get(nodeParams.getItemKey());
                    if (val != null) {
                        result.add(val);
                    }
                }
            });
            nodeContext.put(flowNode.getCode(), result);
        } else {
            throw new IllegalArgumentException("""
                    The value of the data source variable {0} for Extract-Transform Node {1} is not a collection: {2}.
                    """, nodeParams.getCollectionVariable(), flowNode.getName(), variableValue);
        }
    }
}
