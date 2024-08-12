package info.openmeta.starter.flow.action;

import info.openmeta.framework.base.exception.FlowException;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.exception.JSONException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.ActionExceptionSignal;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.Collection;
import java.util.Map;

/**
 * Action exception resolver, check and process the action result exception according to the exception policy.
 */
@Slf4j
public class ActionExceptionResolver {

    private ActionExceptionResolver() {}

    /**
     * Check and process the action result exception,
     * covering null, false, empty string, empty collection, empty map, and empty value,
     * and then determine the exception signal to be thrown.
     *
     * @param flowAction Flow action
     * @param actionContext Action context
     */
    public static void resolveActionException(FlowAction flowAction, ActionContext actionContext) {
        ActionExceptionPolicy exceptionPolicy = getActionExceptionPolicy(flowAction);
        if (exceptionPolicy == null) {
            return;
        }
        Object value = actionContext.get(flowAction.getCode());
        boolean throwExceptionSignal = checkResultForException(value, exceptionPolicy);
        if (!throwExceptionSignal) {
            return;
        }
        ActionExceptionSignal exceptionSignal = exceptionPolicy.getExceptionSignal();
        // Exception message interpolation calculation
        String exceptionMessage = ComputeUtils.stringInterpolation(exceptionPolicy.getExceptionMessage(), actionContext.getEnv());
        // Process exception signal
        processExceptionSignal(flowAction, exceptionSignal, exceptionMessage, actionContext);
    }

    /**
     * Process the exception signal.
     *
     * @param flowAction Flow action
     * @param exceptionSignal Exception signal
     * @param exceptionMessage Exception message
     * @param actionContext Action context
     */
    public static void processExceptionSignal(FlowAction flowAction, ActionExceptionSignal exceptionSignal,
                                              String exceptionMessage, ActionContext actionContext) {
        if (ActionExceptionSignal.THROW_EXCEPTION.equals(exceptionSignal)) {
            throw new FlowException("The result of flow action {0} is an exception: {1}",
                    flowAction.getName(), exceptionMessage);
        } else {
            actionContext.setExceptionSignal(exceptionSignal);
            if (StringUtils.isNotBlank(exceptionMessage)) {
                log.warn("The result of flow action {} is an exception: {}, will {}",
                        flowAction.getName(), exceptionMessage, exceptionSignal.getName());
            }
        }
    }

    /**
     * Extract the action exception policy and verify the parameter configuration of the exception policy.
     *
     * @param flowAction Flow action
     * @return Action exception policy
     */
    private static ActionExceptionPolicy getActionExceptionPolicy(FlowAction flowAction) {
        if (flowAction.getExceptionPolicy() == null) {
            return null;
        }
        ActionExceptionPolicy exceptionPolicy;
        try {
            exceptionPolicy = JsonMapper.jsonNodeToObject(flowAction.getExceptionPolicy(), ActionExceptionPolicy.class);
        } catch (JSONException e) {
            throw new JSONException("The exception handling strategy for flow action {0} encountered an exception: {1}",
                    flowAction.getName(), flowAction, e);
        }
        if (exceptionPolicy == null) {
            return null;
        }
        Assert.notNull(exceptionPolicy.getExceptionWhen(),
                "The `exceptionWhen` of exception handling strategy cannot be empty: {0}", exceptionPolicy);
        Assert.notNull(exceptionPolicy.getExceptionSignal(),
                "The `exceptionSignal` of exception handling strategy cannot be empty: {0}", exceptionPolicy);
        if (ActionExceptionSignal.THROW_EXCEPTION.equals(exceptionPolicy.getExceptionSignal())) {
            Assert.notBlank(exceptionPolicy.getExceptionMessage(),
                    "The exception handling strategy is configured to throw an exception, " +
                            "so the exception message configuration cannot be empty: {0}", exceptionPolicy);
        }
        return exceptionPolicy;
    }

    /**
     * According to the action exception policy, determine whether the action execution result triggers an exception.
     *
     * @param value Action execution result
     * @param exceptionPolicy Action exception policy configuration
     * @return Whether to throw an exception
     */
    private static boolean checkResultForException(Object value, ActionExceptionPolicy exceptionPolicy) {
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
     * Check whether the Action execution result is empty.
     *
     * @param value Action execution result value
     * @return Whether the result is empty
     */
    private static boolean checkResultIsEmpty(Object value) {
        return value == null || (value instanceof String && ((String) value).isEmpty()
                || (value instanceof Collection && ((Collection<?>) value).isEmpty()))
                || (value instanceof Map && ((Map<?, ?>) value).isEmpty());
    }

}
