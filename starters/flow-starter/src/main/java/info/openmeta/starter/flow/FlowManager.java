package info.openmeta.starter.flow;

import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.jdbc.JdbcService;
import info.openmeta.starter.flow.entity.FlowConfig;
import info.openmeta.starter.flow.entity.FlowTrigger;
import info.openmeta.starter.flow.enums.TriggerEventType;
import jakarta.validation.constraints.NotNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import static info.openmeta.framework.base.enums.AccessType.*;

/**
 * Flow manager, store all triggers and corresponding flow configs,
 * and provide some public methods to quickly find triggers and flow configurations based on conditions.
 * Load all Flow configs at once during initialization.
 * TODO: cache FlowConfig and FlowTrigger in Redis
 */
@Slf4j
@Component
public class FlowManager implements InitializingBean {

    // { triggerModel: [FlowTrigger] }
    private static final Map<String, List<FlowTrigger>> MODEL_TRIGGER_MAP = new ConcurrentHashMap<>();

    // { cronId: FlowTrigger }
    private static final Map<Long, FlowTrigger> CRON_TRIGGER_MAP = new ConcurrentHashMap<>();

    // { flowId: FlowConfig }
    private static final Map<Long, FlowConfig> FLOW_MAP = new ConcurrentHashMap<>();

    @Autowired
    private JdbcService<?> jdbcService;

    /**
     * Initialize Bean, read all FlowTrigger and FlowConfig, and maintain them as Map structure
     */
    @Override
    public void afterPropertiesSet() {
        MODEL_TRIGGER_MAP.clear();
        CRON_TRIGGER_MAP.clear();
        FLOW_MAP.clear();
        // Set the initial Context
        ContextHolder.setContext(new Context());
        // Load flow triggers
        List<FlowTrigger> triggers = jdbcService.selectMetaEntityList(FlowTrigger.class, null);
        triggers.forEach(trigger -> {
            MODEL_TRIGGER_MAP.computeIfAbsent(trigger.getTriggeredModel(), k -> new ArrayList<>()).add(trigger);
            if (trigger.getCronId() != null && trigger.getCronId() > 0) {
                CRON_TRIGGER_MAP.put(trigger.getCronId(), trigger);
            }
        });
        // Load flow configs
        List<FlowConfig> flowConfigs = jdbcService.selectMetaEntityList(FlowConfig.class, null);
        flowConfigs.forEach(flowConfig -> FLOW_MAP.put(flowConfig.getId(), flowConfig));
    }

    /**
     * Get FlowConfig by flowId
     * @param flowId flowId
     * @return FlowConfig
     */
    public static FlowConfig getFlowById(Long flowId) {
        return FLOW_MAP.get(flowId);
    }

    /**
     * Get FlowTrigger by cronId
     *
     * @param cronId cronId
     * @return FlowTrigger
     */
    public static FlowTrigger getTriggerByCronId(Long cronId) {
        return CRON_TRIGGER_MAP.get(cronId);
    }

    /**
     * Get all FlowTriggers by triggerModel
     *
     * @param triggerModel triggerModel
     * @return List of FlowTrigger
     */
    private static List<FlowTrigger> getModelFlowTriggers(String triggerModel) {
        List<FlowTrigger> flowTriggers = MODEL_TRIGGER_MAP.get(triggerModel);
        Assert.notEmpty(flowTriggers, "The model {0} does not define any triggers!", triggerModel);
        return flowTriggers;
    }

    /**
     * Get FlowTrigger by triggerModel and triggerCode
     *
     * @param triggerModel triggerModel
     * @param triggerCode triggerCode
     * @return FlowTrigger
     */
    public static FlowTrigger getTriggerByCode(String triggerModel, @NotNull String triggerCode) {
        List<FlowTrigger> flowTriggers = getModelFlowTriggers(triggerModel);
        for (FlowTrigger flowTrigger : flowTriggers) {
            if (triggerCode.equals(flowTrigger.getTriggerCode())) {
                return flowTrigger;
            }
        }
        throw new IllegalArgumentException("The model {0} does not define a trigger with code {1}!", triggerModel, triggerCode);
    }

    /**
     * Get FlowTrigger by triggerModel, triggerCode and eventType
     *
     * @param triggerModel triggerModel
     * @param triggerCode triggerCode
     * @param eventType eventType
     * @return FlowTrigger
     */
    public static FlowTrigger getTriggerByCode(String triggerModel, @NotNull String triggerCode, @NotNull TriggerEventType eventType) {
        List<FlowTrigger> flowTriggers = getModelFlowTriggers(triggerModel);
        for (FlowTrigger flowTrigger : flowTriggers) {
            if (triggerCode.equals(flowTrigger.getTriggerCode()) && eventType.equals(flowTrigger.getEventType())) {
                return flowTrigger;
            }
        }
        throw new IllegalArgumentException("The model {0} does not define a {1} trigger with code {2}!",
                triggerModel, eventType.getType(), triggerCode);
    }

    /**
     * Get FlowTriggers by triggerModel, updateFields and AccessType
     *
     * @param triggerModel triggerModel
     * @param accessType accessType
     * @param updateFields updateFields
     * @return List of FlowTrigger
     */
    public static List<FlowTrigger> getTriggersByChangeEvent(String triggerModel, @NotNull AccessType accessType, Set<String> updateFields) {
        List<FlowTrigger> flowTriggers = MODEL_TRIGGER_MAP.get(triggerModel);
        if (CollectionUtils.isEmpty(flowTriggers)) {
            return Collections.emptyList();
        }
        return flowTriggers.stream().filter(flowTrigger -> isTriggerMatch(flowTrigger, accessType, updateFields)).collect(Collectors.toList());
    }

    /**
     * Determine whether the change event matches the trigger
     *
     * @param flowTrigger flowTrigger
     * @param accessType accessType
     * @param updateFields updateFields
     * @return boolean
     */
    private static boolean isTriggerMatch(FlowTrigger flowTrigger, AccessType accessType, Set<String> updateFields) {
        TriggerEventType eventType = flowTrigger.getEventType();
        if (TriggerEventType.CHANGED_EVENT.equals(eventType)) {
            // Trigger event: CREATE_EVENT, UPDATE_EVENT, DELETE_EVENT
            return true;
        }
        if (CREATE.equals(accessType)) {
            // CREATE event: CREATE_EVENT, CREATE_OR_UPDATE
            return TriggerEventType.CREATE_EVENT.equals(eventType)
                    || TriggerEventType.CREATE_OR_UPDATE.equals(eventType);
        } else if (UPDATE.equals(accessType)) {
            // UPDATE event: UPDATE_EVENT, CREATE_OR_UPDATE, and the triggered fields are empty,
            // or any of the updated fields are in the triggered fields
            if (CollectionUtils.isEmpty(flowTrigger.getTriggeredFields())
                    || CollectionUtils.isEmpty(updateFields)
                    || updateFields.stream().anyMatch(flowTrigger.getTriggeredFields()::contains)) {
                return TriggerEventType.UPDATE_EVENT.equals(eventType)
                        || TriggerEventType.CREATE_OR_UPDATE.equals(eventType);
            } else {
                return false;
            }
        } else if (DELETE.equals(accessType)) {
            // DELETE event: DELETE_EVENT
            return TriggerEventType.DELETE_EVENT.equals(eventType);
        } else {
            return false;
        }
    }

}
