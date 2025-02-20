package info.openmeta.starter.flow.service;

import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.starter.flow.entity.FlowConfig;
import info.openmeta.starter.flow.message.dto.FlowEventMessage;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * FlowConfig Model Service Interface
 */
public interface FlowConfigService extends EntityService<FlowConfig, String> {

    /**
     * Get the flow list by model name.
     *
     * @param modelName model name
     * @return flow configuration list
     */
    List<Map<String, Object>> getByModel(String modelName);

    /**
     * Get the flowConfig by ID, including nodes and edges.
     *
     * @param flowId flow ID
     * @return flowConfig object with nodes and edges
     */
    Optional<FlowConfig> getFlowById(String flowId);

    /**
     * Execute a non-transactional flow according to the FlowEventMessage.
     *
     * @param eventMessage Flow event message
     * @return Flow execution result
     */
    Object executeFlow(FlowEventMessage eventMessage);

    /**
     * Trigger a transactional Flow according to the FlowEventMessage.
     * The Flow runs in a transaction, and the transaction is rolled back when an exception is thrown internally.
     *
     * @param eventMessage Flow event message
     * @return Flow execution result
     */
    Object executeTransactionalFlow(FlowEventMessage eventMessage);
}