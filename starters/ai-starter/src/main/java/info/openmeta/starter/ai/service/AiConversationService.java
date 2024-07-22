package info.openmeta.starter.ai.service;

import info.openmeta.starter.ai.entity.AiConversation;
import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.starter.ai.vo.ChatMessage;

/**
 * AiConversation Model Service Interface
 */
public interface AiConversationService extends EntityService<AiConversation, Long> {

    /**
     * Get the conversation object based on the chat message.
     * If there is no conversation ID, create a new conversation object.
     *
     * @param chatMessage Chat message
     */
    void handleConversation(ChatMessage chatMessage);
}