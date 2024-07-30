package info.openmeta.starter.flow.service;

import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.entity.FlowAction;

/**
 * FlowAction Model Service Interface
 */
public interface FlowActionService extends EntityService<FlowAction, Long> {

    /**
     * Process flow action
     * @param flowAction flow action
     * @param actionContext environment variables for executing flow actions, including row data that trigger the flow
     */
    void processFlowAction(FlowAction flowAction, ActionContext actionContext);

}