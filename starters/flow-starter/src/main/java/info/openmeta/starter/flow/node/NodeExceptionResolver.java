package info.openmeta.starter.flow.node;

import info.openmeta.framework.base.exception.FlowException;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.exception.JSONException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.NodeExceptionSignal;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.Collection;
import java.util.Map;

/**
 * Node exception resolver, check and process the node result exception according to the exception policy.
 */
@Slf4j
public class NodeExceptionResolver {

    private NodeExceptionResolver() {}

    /**
     * Check and process the node result exception,
     * covering null, false, empty string, empty collection, empty map, and empty value,
     * and then determine the exception signal to be thrown.
     *
     * @param flowNode Flow node
     * @param nodeContext Node context
     */
    public static void resolveNodeException(FlowNode flowNode, NodeContext nodeContext) {
        NodeExceptionPolicy exceptionPolicy = getNodeExceptionPolicy(flowNode);
        if (exceptionPolicy == null) {
            return;
        }
        Object value = nodeContext.get(flowNode.getId());
        boolean throwExceptionSignal = checkResultForException(value, exceptionPolicy);
        if (!throwExceptionSignal) {
            return;
        }
        NodeExceptionSignal exceptionSignal = exceptionPolicy.getExceptionSignal();
        // Exception message interpolation calculation
        String exceptionMessage = ComputeUtils.stringInterpolation(exceptionPolicy.getExceptionMessage(), nodeContext.getEnv());
        // Process exception signal
        processExceptionSignal(flowNode, exceptionSignal, exceptionMessage, nodeContext);
    }

    /**
     * Process the exception signal.
     *
     * @param flowNode Flow node
     * @param exceptionSignal Exception signal
     * @param exceptionMessage Exception message
     * @param nodeContext Node context
     */
    public static void processExceptionSignal(FlowNode flowNode, NodeExceptionSignal exceptionSignal,
                                              String exceptionMessage, NodeContext nodeContext) {
        if (NodeExceptionSignal.THROW_EXCEPTION.equals(exceptionSignal)) {
            throw new FlowException("The result of flow node {0} is an exception: {1}",
                    flowNode.getName(), exceptionMessage);
        } else {
            nodeContext.setExceptionSignal(exceptionSignal);
            if (StringUtils.isNotBlank(exceptionMessage)) {
                log.warn("The result of flow node {} is an exception: {}, will {}",
                        flowNode.getName(), exceptionMessage, exceptionSignal.getName());
            }
        }
    }

    /**
     * Extract the node exception policy and verify the parameter configuration of the exception policy.
     *
     * @param flowNode Flow node
     * @return Node exception policy
     */
    private static NodeExceptionPolicy getNodeExceptionPolicy(FlowNode flowNode) {
        if (flowNode.getExceptionPolicy() == null) {
            return null;
        }
        NodeExceptionPolicy exceptionPolicy;
        try {
            exceptionPolicy = JsonMapper.jsonNodeToObject(flowNode.getExceptionPolicy(), NodeExceptionPolicy.class);
        } catch (JSONException e) {
            throw new JSONException("The exception handling strategy for flow node {0} encountered an exception: {1}",
                    flowNode.getName(), flowNode, e);
        }
        if (exceptionPolicy == null) {
            return null;
        }
        Assert.notNull(exceptionPolicy.getExceptionWhen(),
                "The `exceptionWhen` of exception handling strategy cannot be empty: {0}", exceptionPolicy);
        Assert.notNull(exceptionPolicy.getExceptionSignal(),
                "The `exceptionSignal` of exception handling strategy cannot be empty: {0}", exceptionPolicy);
        if (NodeExceptionSignal.THROW_EXCEPTION.equals(exceptionPolicy.getExceptionSignal())) {
            Assert.notBlank(exceptionPolicy.getExceptionMessage(),
                    "The exception handling strategy is configured to throw an exception, " +
                            "so the exception message configuration cannot be empty: {0}", exceptionPolicy);
        }
        return exceptionPolicy;
    }

    /**
     * According to the node exception policy, determine whether the node execution result triggers an exception.
     *
     * @param value Node execution result
     * @param exceptionPolicy Node exception policy configuration
     * @return Whether to throw an exception
     */
    private static boolean checkResultForException(Object value, NodeExceptionPolicy exceptionPolicy) {
        boolean throwExceptionSignal;
        switch (exceptionPolicy.getExceptionWhen()) {
            case RESULT_IS_EMPTY:
                throwExceptionSignal = checkResultIsEmpty(value);
                break;
            case RESULT_IS_EMPTY_OR_ZERO:
                throwExceptionSignal = checkResultIsEmpty(value) || value.equals(0) || value.equals(0L);
                break;
            case RESULT_IS_NOT_EMPTY:
                throwExceptionSignal = !checkResultIsEmpty(value);
                break;
            case RESULT_IS_FALSE:
                throwExceptionSignal = value == null || Boolean.FALSE.equals(value);
                break;
            case RESULT_IS_TRUE:
                throwExceptionSignal = Boolean.TRUE.equals(value);
                break;
            default:
                throw new IllegalArgumentException("Unsupported exception strategy condition: {0}",
                        exceptionPolicy.getExceptionWhen());
        }
        return throwExceptionSignal;
    }

    /**
     * Check whether the Node execution result is empty.
     *
     * @param value Node execution result value
     * @return Whether the result is empty
     */
    private static boolean checkResultIsEmpty(Object value) {
        return value == null || (value instanceof String && ((String) value).isEmpty()
                || (value instanceof Collection && ((Collection<?>) value).isEmpty()))
                || (value instanceof Map && ((Map<?, ?>) value).isEmpty());
    }

}
