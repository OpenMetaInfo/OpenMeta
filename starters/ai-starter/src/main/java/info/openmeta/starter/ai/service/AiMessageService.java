package info.openmeta.starter.ai.service;

import com.plexpt.chatgpt.entity.billing.Usage;
import info.openmeta.starter.ai.entity.AiMessage;
import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.starter.ai.vo.ChatMessage;

/**
 * AiMessage Model Service Interface
 */
public interface AiMessageService extends EntityService<AiMessage, Long> {

    /**
     * Save AI message, including query content, response content, and token consumption.
     *
     * @param chatMessage Chat message
     * @param answer Answer content
     * @param usage Token usage statistics
     */
    AiMessage saveAiMessage(ChatMessage chatMessage, String answer, Usage usage);
}