package info.openmeta.starter.flow.service.impl;

import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionFactory;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.service.FlowActionService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * FlowAction Model Service Implementation
 */
@Service
public class FlowActionServiceImpl extends EntityServiceImpl<FlowAction, Long> implements FlowActionService {

    @Autowired
    private ActionFactory<?> actionFactory;

    /**
     * Verify whether the condition expression is met, and then determine whether to execute the current node or action.
     * The Flow Engine has set the env to be unmodifiable to avoid data pollution
     *
     * @param condition condition expression
     * @param actionContext flow environment variables
     * @return whether to execute
     */
    private boolean isValidActionCondition(String condition, ActionContext actionContext) {
        // When the condition expression is not empty, determine whether to execute the current node or action.
        if (StringUtils.isNotBlank(condition)) {
            return ComputeUtils.executeBoolean(condition, actionContext.getEnv());
        }
        return true;
    }

    /**
     * Process flow action
     * @param flowAction flow action
     * @param actionContext environment variables for executing flow actions, including row data that trigger the flow
     */
    @Override
    public void processFlowAction(FlowAction flowAction, ActionContext actionContext) {
        // Skip the current action and continue to the next action when the execution condition is not met
        if (!isValidActionCondition(flowAction.getActionCondition(), actionContext)) {
            actionContext.put(flowAction.getCode(), null);
            return;
        }
        actionFactory.executeActionProcessor(flowAction, actionContext);
    }
}