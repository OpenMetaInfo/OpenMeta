package info.openmeta.starter.flow.service;

import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.entity.FlowNode;

/**
 * FlowNode Model Service Interface
 */
public interface FlowNodeService extends EntityService<FlowNode, Long> {

    /**
     * Process flow node.
     * The actions in the same node are processed in the same transaction.
     *
     * @param flowNode flow node
     * @param actionContext environment variables for executing flow actions, including row data that trigger the flow
     */
    void processFlowNode(FlowNode flowNode, ActionContext actionContext);

}