package info.openmeta.starter.flow;

import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.exception.IllegalArgumentException;
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

    // { triggerId: FlowTrigger }
    private static final Map<String, FlowTrigger> TRIGGER_MAP = new ConcurrentHashMap<>();

    // { sourceModel: [TriggerId] }
    private static final Map<String, List<String>> MODEL_TRIGGER_MAP = new ConcurrentHashMap<>();

    // { cronId: TriggerId }
    private static final Map<Long, String> CRON_TRIGGER_MAP = new ConcurrentHashMap<>();

    // { flowId: FlowConfig }
    private static final Map<String, FlowConfig> FLOW_MAP = new ConcurrentHashMap<>();

    @Autowired
    private JdbcService<?> jdbcService;

    /**
     * Initialize Bean, read all FlowTrigger and FlowConfig, and maintain them as Map structure
     */
    @Override
    public void afterPropertiesSet() {
        TRIGGER_MAP.clear();
        MODEL_TRIGGER_MAP.clear();
        CRON_TRIGGER_MAP.clear();
        FLOW_MAP.clear();
        // Set the initial Context
        ContextHolder.setContext(new Context());
        // Load flow triggers
        List<FlowTrigger> triggers = jdbcService.selectMetaEntityList(FlowTrigger.class, null);
        triggers.forEach(trigger -> {
            TRIGGER_MAP.put(trigger.getId(), trigger);
            MODEL_TRIGGER_MAP.computeIfAbsent(trigger.getSourceModel(), k -> new ArrayList<>()).add(trigger.getId());
            if (trigger.getCronId() != null && trigger.getCronId() > 0) {
                CRON_TRIGGER_MAP.put(trigger.getCronId(), trigger.getId());
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
    public static FlowConfig getById(String flowId) {
        return FLOW_MAP.get(flowId);
    }

    /**
     * Get FlowTrigger by cronId
     *
     * @param cronId cronId
     * @return FlowTrigger
     */
    public static FlowTrigger getTriggerByCronId(Long cronId) {
        if (CRON_TRIGGER_MAP.containsKey(cronId)) {
            return TRIGGER_MAP.get(CRON_TRIGGER_MAP.get(cronId));
        } else {
            return null;
        }
    }

    /**
     * Get all FlowTriggers by sourceModel
     *
     * @param sourceModel sourceModel
     * @return List of FlowTrigger
     */
    private static List<FlowTrigger> getModelTriggers(String sourceModel) {
        List<String> triggerIds = MODEL_TRIGGER_MAP.get(sourceModel);
        if (CollectionUtils.isEmpty(triggerIds)) {
            return Collections.emptyList();
        }
        return triggerIds.stream().map(TRIGGER_MAP::get).collect(Collectors.toList());
    }

    /**
     * Get FlowTrigger by triggerId
     *
     * @param triggerId triggerId
     * @return FlowTrigger object
     */
    public static FlowTrigger getTriggerById(@NotNull String triggerId) {
        return TRIGGER_MAP.get(triggerId);
    }

    /**
     * Get FlowTrigger by sourceModel, triggerId and eventType
     *
     * @param sourceModel sourceModel
     * @param triggerId triggerId
     * @param eventType eventType
     * @return FlowTrigger
     */
    public static FlowTrigger getTriggerById(String sourceModel, @NotNull String triggerId, @NotNull TriggerEventType eventType) {
        List<FlowTrigger> flowTriggers = getModelTriggers(sourceModel);
        for (FlowTrigger flowTrigger : flowTriggers) {
            if (triggerId.equals(flowTrigger.getId()) && eventType.equals(flowTrigger.getEventType())) {
                return flowTrigger;
            }
        }
        throw new IllegalArgumentException("The model {0} does not define a {1} trigger with triggerId {2}!",
                sourceModel, eventType.getType(), triggerId);
    }

    /**
     * Get FlowTriggers by sourceModel, updateFields and AccessType
     *
     * @param sourceModel sourceModel
     * @param accessType accessType
     * @param updateFields updateFields
     * @return List of FlowTrigger
     */
    public static List<FlowTrigger> getTriggersByChangeEvent(String sourceModel, @NotNull AccessType accessType, Set<String> updateFields) {
        List<FlowTrigger> flowTriggers = getModelTriggers(sourceModel);
        if (CollectionUtils.isEmpty(flowTriggers)) {
            return Collections.emptyList();
        }
        return flowTriggers.stream()
                .filter(flowTrigger -> isTriggerMatch(flowTrigger, accessType, updateFields))
                .toList();
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
            if (CollectionUtils.isEmpty(flowTrigger.getSourceFields())
                    || CollectionUtils.isEmpty(updateFields)
                    || updateFields.stream().anyMatch(flowTrigger.getSourceFields()::contains)) {
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
