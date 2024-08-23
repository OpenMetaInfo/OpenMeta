package info.openmeta.starter.flow.action;

import info.openmeta.framework.base.exception.FlowException;
import info.openmeta.framework.base.exception.JSONException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.starter.flow.action.params.ActionParams;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Flow action factory
 */
@Slf4j
@Component
public class ActionFactory<T extends ActionParams> {

    private final Map<FlowActionType, ActionProcessor<T>> ACTION_PROCESSOR_MAP = new HashMap<>();

    /**
     * Inject all actions through the constructor
     *
     * @param actionProcessors Spring automatic assembly collection,
     *                         automatically collect all implementations of ActionProcessor<T>
     */
    @Autowired
    public ActionFactory(List<ActionProcessor<T>> actionProcessors) {
        for (ActionProcessor<T> actionProcessor : actionProcessors) {
            ACTION_PROCESSOR_MAP.put(actionProcessor.getActionType(), actionProcessor);
        }
    }

    /**
     * Execute the flow action processor
     *
     * @param flowAction    Flow action
     * @param actionContext Environment variables for executing flow actions,
     *                      including row data that trigger the flow
     */
    public void executeActionProcessor(FlowAction flowAction, ActionContext actionContext) {
        // Get the action processor by action type
        ActionProcessor<T> actionProcessor = ACTION_PROCESSOR_MAP.get(flowAction.getActionType());
        Assert.notNull(actionProcessor, "The processor for flow action {0} does not exist!", flowAction.getActionType());
        // Get the action parameter, and convert the action parameters to the corresponding parameter type instance
        Class<T> paramsClass = actionProcessor.getParamsClass();
        T actionParams;
        try {
            actionParams = JsonMapper.jsonNodeToObject(flowAction.getActionParams(), paramsClass);
        } catch (JSONException e) {
            throw new JSONException("Failed to convert the parameters of flow action {0} to an instance of {1}: {2}",
                    flowAction, paramsClass.getSimpleName(), e.getMessage());
        }
        Assert.notNull(actionParams, "The parameters for flow action {0} cannot be null", flowAction);
        actionProcessor.validateParams(flowAction, actionParams);
        try {
            actionProcessor.execute(flowAction, actionParams, actionContext);
        } catch (DuplicateKeyException e) {
            throw new FlowException("Failed to execute flow action due to unique index constraint violation: {0} ",
                    flowAction, e);
        } catch (Exception e) {
            throw new FlowException("Failed to execute flow action {0}! \n ActionParams: {} \n ActionContext: {}",
                    flowAction, actionParams, actionContext, e);
        }
        // Resolve and handle the exception result of FlowAction
        ActionExceptionResolver.resolveActionException(flowAction, actionContext);
    }

}
