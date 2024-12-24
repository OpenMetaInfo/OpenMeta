package info.openmeta.starter.flow.service.impl;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.flow.FlowEnv;
import info.openmeta.starter.flow.constant.FlowConstant;
import info.openmeta.starter.flow.entity.*;
import info.openmeta.starter.flow.enums.FlowStatus;
import info.openmeta.starter.flow.enums.FlowType;
import info.openmeta.starter.flow.enums.NodeExceptionSignal;
import info.openmeta.starter.flow.message.dto.FlowEventMessage;
import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.service.FlowConfigService;
import info.openmeta.starter.flow.service.FlowEventService;
import info.openmeta.starter.flow.service.FlowInstanceService;
import info.openmeta.starter.flow.service.FlowNodeService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StopWatch;

/**
 * FlowConfig Model Service Implementation
 */
@Slf4j
@Service
public class FlowConfigServiceImpl extends EntityServiceImpl<FlowConfig, Long> implements FlowConfigService {

    @Autowired
    private FlowInstanceService flowInstanceService;
    @Autowired
    private FlowNodeService flowNodeService;

    @Autowired
    private FlowEventService flowEventService;

    /**
     * Get the flow configuration, including the node list and the node list in the node.
     *
     * @param flowId flow configuration ID
     * @return flow configuration
     */
    private FlowConfig getFlowDefinition(Long flowId) {
        FlowConfig flowConfig = this.getById(flowId);
        Filters filters = new Filters().eq(FlowNode::getFlowId, flowId);
        // Sort FlowNode in ascending order according to the `sequence` of the node.
        Orders orders = Orders.ofAsc(FlowNode::getSequence);
        flowConfig.setNodeList(flowNodeService.searchList(new FlexQuery(filters, orders)));
        return flowConfig;
    }

    /**
     * Execute a non-transactional flow according to the FlowEventMessage.
     *
     * @param eventMessage Flow event message
     * @return Flow execution result
     */
    @Override
    public Object executeFlow(FlowEventMessage eventMessage) {
        // TODO: Add the validation of the `scope` of the flow.
        // Filters scope = flowConfig.getScope();
        FlowConfig flowDefinition = this.getFlowDefinition(eventMessage.getFlowId());
        StopWatch stopWatch = new StopWatch("Executing flow：" + flowDefinition.getName());
        // Add the row data that triggers the flow to the environment variables
        NodeContext nodeContext = new NodeContext(FlowEnv.getEnv());
        nodeContext.put(FlowConstant.TRIGGER_ROW_ID, eventMessage.getTriggerRowId());
        nodeContext.put(FlowConstant.TRIGGER_PARAMS, eventMessage.getTriggerParams());
        for (FlowNode flowNode : flowDefinition.getNodeList()) {
            stopWatch.start(flowNode.getNodeType().getType() + " - " + flowNode.getName());
            flowNodeService.processFlowNode(flowNode, nodeContext);
            stopWatch.stop();
            if (NodeExceptionSignal.END_FLOW.equals(nodeContext.getExceptionSignal())) {
                // End the flow, exit directly, do not execute subsequent nodes, and do not roll back the executed nodes.
                log.info(stopWatch.prettyPrint());
                return null;
            }
        }
        log.warn(stopWatch.prettyPrint());
        return nodeContext.getReturnData();
    }

    /**
     * Trigger a transactional Flow according to the FlowEventMessage.
     * The Flow runs in a transaction, and the transaction is rolled back when an exception is thrown internally.
     *
     * @param eventMessage Flow event message
     * @return Flow execution result
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Object executeTransactionalFlow(FlowEventMessage eventMessage) {
        return executeFlow(eventMessage);
    }

    /**
     * Prepare flow data, initialize flow instance
     *
     * @param flowDefinition flow definition
     * @param eventMessage flow event message
     */
    private void prepareFlowData(FlowConfig flowDefinition, FlowEventMessage eventMessage) {
        if (FlowType.VALIDATION_FLOW.equals(flowDefinition.getFlowType())) {
            // Validation flow does not create flow instances and flow events
            return;
        }
        FlowEvent flowEvent = this.createFlowEvent(eventMessage);
        FlowInstance flowInstance = this.initFlowInstance(flowEvent);
    }

    /**
     * Create a flow event
     *
     * @param eventMessage event message
     * @return flow event
     */
    private FlowEvent createFlowEvent(FlowEventMessage eventMessage) {
        FlowEvent flowEvent = new FlowEvent();
        flowEvent.setFlowId(eventMessage.getFlowId());
        flowEvent.setFlowNodeId(eventMessage.getFlowNodeId());
        flowEvent.setFlowModel(eventMessage.getFlowModel());
        String rowId = eventMessage.getTriggerRowId() == null ? null : eventMessage.getTriggerRowId().toString();
        flowEvent.setRowId(rowId);
        flowEvent.setTriggerId(eventMessage.getTriggerId());
        flowEvent.setTriggeredModel(eventMessage.getTriggeredModel());
        return flowEventService.createOneAndFetch(flowEvent);
    }

    /**
     * Initialize FlowInstance
     *
     * @param flowEvent flow event
     * @return initialized FlowInstance
     */
    private FlowInstance initFlowInstance(FlowEvent flowEvent) {
        FlowInstance flowInstance = new FlowInstance();
        flowInstance.setModel(flowEvent.getFlowModel());
        flowInstance.setRowId(flowEvent.getRowId());
        flowInstance.setFlowId(flowEvent.getFlowId());
        flowInstance.setTriggerId(flowEvent.getTriggerId());
        flowInstance.setCurrentStatus(FlowStatus.INITIAL);
        return flowInstanceService.createOneAndFetch(flowInstance);
    }

}