package info.openmeta.starter.flow;

import info.openmeta.framework.base.constant.TimeConstant;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.changelog.message.dto.ChangeLog;
import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.framework.orm.utils.MapUtils;
import info.openmeta.starter.cron.message.dto.CronTaskMessage;
import info.openmeta.starter.flow.entity.FlowConfig;
import info.openmeta.starter.flow.entity.FlowTrigger;
import info.openmeta.starter.flow.enums.TriggerEventType;
import info.openmeta.starter.flow.message.FlowEventProducer;
import info.openmeta.starter.flow.message.dto.FlowEventMessage;
import info.openmeta.starter.flow.service.FlowConfigService;
import info.openmeta.starter.flow.vo.TriggerEventVO;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;

/**
 * Flow automation service.
 */
@Slf4j
@Component
public class FlowAutomation {

    @Autowired
    private FlowEventProducer flowEventProducer;

    @Autowired
    private FlowConfigService flowConfigService;

    /**
     * Trigger synchronous flows by changeLogs.
     * @param changeLogs The list of change logs.
     */
    @Transactional(rollbackFor = Exception.class)
    public void triggerSyncFlows(List<ChangeLog> changeLogs) {
        triggerFlowByChangeLog(changeLogs, true);
    }

    /**
     * Trigger asynchronous flows by changeLogs, send to MQ.
     * @param changeLogs The list of change logs.
     */
    public void triggerAsyncFlows(List<ChangeLog> changeLogs) {
        triggerFlowByChangeLog(changeLogs, false);
    }

    /**
     * Trigger flows by changeLogs.
     * @param changeLogs The list of change logs.
     * @param sync Whether to trigger synchronous flows.
     */
    private List<FlowEventMessage> getEventMessagesByChangeLogs(List<ChangeLog> changeLogs, boolean sync) {
        List<FlowEventMessage> flowEventMessages = new ArrayList<>();
        for (ChangeLog changeLog : changeLogs) {
            String sourceModel = changeLog.getModel();
            Serializable triggerRowId = changeLog.getRowId();
            Set<String> updateFields = AccessType.UPDATE.equals(changeLog.getAccessType()) ? changeLog.getDataAfterChange().keySet() : Collections.emptySet();
            Map<String, Object> triggerParams = AccessType.DELETE.equals(changeLog.getAccessType()) ? changeLog.getDataBeforeChange() : changeLog.getDataAfterChange();
            // Get the triggers of the current model and event
            List<FlowTrigger> flowTriggers = FlowManager.getTriggersByChangeEvent(sourceModel, changeLog.getAccessType(), updateFields);
            flowTriggers.forEach(flowTrigger -> {
                // Validate the trigger execution condition
                if (!this.validateTriggerCondition(flowTrigger, triggerParams)) {
                    log.debug("The trigger condition {} for Trigger {} is not met; the flow will not be triggered!",
                            flowTrigger.getTriggerCondition(), flowTrigger.getId());
                    return;
                }
                FlowConfig flowConfig = FlowManager.getById(flowTrigger.getFlowId());
                Assert.notNull(flowConfig, "Trigger {0} is not yet bound to any flow!", flowTrigger.getId());
                FlowEventMessage eventMessage = wrapperFlowEventMessage(flowTrigger, flowConfig, triggerRowId, triggerParams);
                flowEventMessages.add(eventMessage);
            });
        }
        return flowEventMessages;
    }

    /**
     * Encapsulate the FlowEventMessage object.
     * @param flowTrigger The flow trigger.
     * @param flowConfig The flow configuration.
     * @param rowId The row ID.
     * @param params The trigger parameters.
     * @return The encapsulated FlowEventMessage object.
     */
    private FlowEventMessage wrapperFlowEventMessage(FlowTrigger flowTrigger, FlowConfig flowConfig,
                                                     Serializable rowId, Map<String, Object> params) {
        FlowEventMessage eventMessage = new FlowEventMessage();
        eventMessage.setFlowId(flowConfig.getId());
        eventMessage.setRollbackOnFail(flowConfig.getRollbackOnFail());
        eventMessage.setTriggerId(flowTrigger.getId());
        eventMessage.setSourceModel(flowTrigger.getSourceModel());
        eventMessage.setTriggerRowId(rowId);
        eventMessage.setTriggerParams(params);
        eventMessage.setContext(ContextHolder.getContext());
        return eventMessage;
    }

    /**
     * Trigger synchronous or asynchronous flows by changeLogs.
     * @param changeLogs The list of change logs.
     * @param isSync Whether to trigger synchronous flows.
     */
    private void triggerFlowByChangeLog(List<ChangeLog> changeLogs, boolean isSync) {
        if (CollectionUtils.isEmpty(changeLogs)) {
            return;
        }
        List<FlowEventMessage> flowEventMessages = this.getEventMessagesByChangeLogs(changeLogs, isSync);
        for (FlowEventMessage flowEventMessage : flowEventMessages) {
            triggerFlow(flowEventMessage, isSync);
        }
    }

    /**
     * Trigger the flow, generate flow events one by one when an event triggers multiple flows.
     * @param eventMessage The trigger event.
     * @param isSync Whether to trigger synchronous flows.
     */
    public Object triggerFlow(FlowEventMessage eventMessage, Boolean isSync) {
        if (Boolean.TRUE.equals(isSync)) {
            // Synchronous flow execution
            if (Boolean.TRUE.equals(eventMessage.getRollbackOnFail())) {
                // Transactional flow
                return flowConfigService.executeTransactionalFlow(eventMessage);
            } else {
                return flowConfigService.executeFlow(eventMessage);
            }
        } else {
            // Send asynchronous flow to MQ
            eventMessage.setContext(ContextHolder.getContext());
            flowEventProducer.sendFlowEvent(eventMessage);
            return true;
        }
    }

    /**
     * Button event, trigger the flow.
     *
     * @param triggerEventVO The button event parameters.
     * @return The button event flow execution result Map.
     */
    public Object buttonEvent(TriggerEventVO triggerEventVO) {
        TriggerEventType eventType = TriggerEventType.BUTTON_EVENT;
        String sourceModel = triggerEventVO.getSourceModel();
        String triggerId = triggerEventVO.getTriggerId();
        FlowTrigger flowTrigger = FlowManager.getTriggerById(sourceModel, triggerId, eventType);
        Assert.isTrue(this.validateTriggerCondition(flowTrigger, triggerEventVO.getEventParams()),
                "The trigger condition {0} for Button Trigger {1} is not met; the flow will not be triggered!",
                flowTrigger.getTriggerCondition(), triggerId);
        FlowConfig flowConfig = FlowManager.getById(flowTrigger.getFlowId());
        Assert.notNull(flowConfig, "Button Trigger {0} is not yet bound to any flow!", triggerId);
        FlowEventMessage eventMessage = wrapperFlowEventMessage(flowTrigger, flowConfig,
                triggerEventVO.getRowId(), triggerEventVO.getEventParams());
        return this.triggerFlow(eventMessage, flowConfig.getSync());
    }

    /**
     * API event, trigger the flow.
     *
     * @param triggerEventVO The API event parameters.
     * @return The API event flow execution result Map.
     */
    public Object apiEvent(TriggerEventVO triggerEventVO) {
        TriggerEventType eventType = TriggerEventType.API_EVENT;
        String triggerModel = triggerEventVO.getSourceModel();
        String triggerId = triggerEventVO.getTriggerId();
        FlowTrigger flowTrigger = FlowManager.getTriggerById(triggerModel, triggerId, eventType);
        Assert.isTrue(this.validateTriggerCondition(flowTrigger, triggerEventVO.getEventParams()),
                "The trigger condition {0} for API Trigger {1} is not met; the flow will not be triggered!",
                flowTrigger.getTriggerCondition(), triggerId);
        FlowConfig flowConfig = FlowManager.getById(flowTrigger.getFlowId());
        Assert.notNull(flowConfig, "API Trigger {0} is not yet bound to any flow!", triggerId);
        FlowEventMessage eventMessage = wrapperFlowEventMessage(flowTrigger, flowConfig,
                triggerEventVO.getRowId(), triggerEventVO.getEventParams());
        return this.triggerFlow(eventMessage, flowConfig.getSync());
    }

    /**
     * Cron event, trigger the cron flow.
     *
     * @param cronTaskMessage The cron task message.
     */
    public void cronEvent(CronTaskMessage cronTaskMessage) {
        FlowTrigger flowTrigger = FlowManager.getTriggerByCronId(cronTaskMessage.getCronId());
        if (flowTrigger == null) {
            return;
        }
        Assert.isTrue(this.validateTriggerCondition(flowTrigger, null),
                "The trigger condition {0} for Cron Trigger {1} is not met; the flow will not be triggered!",
                flowTrigger.getName(), flowTrigger.getTriggerCondition());
        LocalDateTime lastRunTime = cronTaskMessage.getLastExecTime();
        if (lastRunTime == null) {
            // Set the last run time to EPOCH time: 1970-01-01 00:00:00 when it is empty
            lastRunTime = TimeConstant.EPOCH_TIME;
        }
        Map<String, Object> triggerParams = MapUtils.of(CronTaskMessage::getLastExecTime, lastRunTime);
        FlowConfig flowConfig = FlowManager.getById(flowTrigger.getFlowId());
        Assert.notNull(flowConfig, "Cron Trigger {0} is not yet bound to any flow!", flowTrigger.getName());
        // Generate the flow event message triggered by scheduled task, and send it to the MQ.
        FlowEventMessage eventMessage = new FlowEventMessage();
        eventMessage.setFlowId(flowConfig.getId());
        eventMessage.setRollbackOnFail(flowConfig.getRollbackOnFail());
        eventMessage.setTriggerId(flowTrigger.getId());
        eventMessage.setSourceModel(flowTrigger.getSourceModel());
        eventMessage.setTriggerParams(triggerParams);
        eventMessage.setContext(cronTaskMessage.getContext());
        flowEventProducer.sendFlowEvent(eventMessage);
    }

    /**
     * Subflow event.
     *
     * @param triggerId The trigger ID.
     * @param triggerParams The trigger parameters.
     * @return The subflow event result.
     */
    public Object subflowEvent(String triggerId, Map<String, Object> triggerParams) {
        FlowTrigger flowTrigger = FlowManager.getTriggerById(triggerId);
        if (!this.validateTriggerCondition(flowTrigger, triggerParams)) {
            log.debug("The trigger condition {} for Subflow Trigger {} is not met; the flow will not be triggered!",
                    flowTrigger.getTriggerCondition(), triggerId);
            return null;
        }
        FlowConfig flowConfig = FlowManager.getById(flowTrigger.getFlowId());
        Assert.notNull(flowConfig, "Subflow Trigger {0} is not yet bound to any flow!", triggerId);
        FlowEventMessage eventMessage = new FlowEventMessage();
        eventMessage.setFlowId(flowConfig.getId());
        eventMessage.setRollbackOnFail(flowConfig.getRollbackOnFail());
        eventMessage.setTriggerId(flowTrigger.getId());
        eventMessage.setSourceModel(flowTrigger.getSourceModel());
        eventMessage.setTriggerParams(triggerParams);
        eventMessage.setContext(ContextHolder.getContext());
        return this.triggerFlow(eventMessage, flowConfig.getSync());
    }

    /**
     * `onchange` event on field change, causes other field data changes
     *
     * @param triggerEventVO The trigger event parameters.
     * @return The Map of other field data changes caused by field value changes.
     */
    public Map<String, Object> onchangeEvent(TriggerEventVO triggerEventVO) {
        return Collections.emptyMap();
    }

    /**
     * Validate the trigger condition.
     * Not triggered when the condition is false.
     *
     * @param flowTrigger The flow trigger.
     * @param triggerParams The trigger parameters.
     * @return Whether the trigger condition is met.
     */
    private boolean validateTriggerCondition(FlowTrigger flowTrigger, Map<String, Object> triggerParams) {
        if (StringUtils.isNotBlank(flowTrigger.getTriggerCondition())) {
            return ComputeUtils.executeBoolean(flowTrigger.getTriggerCondition(), triggerParams);
        }
        return true;
    }
}
