package info.openmeta.starter.ai.service.impl;

import com.plexpt.chatgpt.entity.billing.Usage;
import info.openmeta.starter.ai.vo.ChatMessage;
import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.ai.entity.AiMessage;
import info.openmeta.starter.ai.service.AiMessageService;
import org.springframework.transaction.annotation.Transactional;

/**
 * AiMessage Model Service Implementation
 */
@Service
public class AiMessageServiceImpl extends EntityServiceImpl<AiMessage, Long> implements AiMessageService {

    /**
     * Save AI message, including query content, response content, and token consumption.
     *
     * @param chatMessage Chat message
     * @param answer Answer content
     * @param usage Token usage statistics
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public AiMessage saveAiMessage(ChatMessage chatMessage, String answer, Usage usage) {
        AiMessage aiMessage = new AiMessage();
        aiMessage.setConversationId(chatMessage.getConversationId());
        aiMessage.setQuery(chatMessage.getContent());
        aiMessage.setAnswer(answer);
        aiMessage.setInputTokens(Math.toIntExact(usage.getPromptTokens()));
        aiMessage.setOutputTokens(Math.toIntExact(usage.getCompletionTokens()));
        aiMessage.setTotalTokens(Math.toIntExact(usage.getTotalTokens()));
        this.createOne(aiMessage);
        return aiMessage;
    }
}