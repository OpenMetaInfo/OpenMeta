package info.openmeta.starter.flow.service;

import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.starter.flow.entity.FlowConfig;
import info.openmeta.starter.flow.message.dto.FlowEventMessage;

/**
 * FlowConfig Model Service Interface
 */
public interface FlowConfigService extends EntityService<FlowConfig, Long> {

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