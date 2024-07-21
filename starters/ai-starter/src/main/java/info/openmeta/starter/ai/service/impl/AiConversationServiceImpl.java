package info.openmeta.starter.ai.service.impl;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.starter.ai.vo.ChatMessage;
import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.ai.entity.AiConversation;
import info.openmeta.starter.ai.service.AiConversationService;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;

/**
 * AiConversation Model Service Implementation
 */
@Service
public class AiConversationServiceImpl extends EntityServiceImpl<AiConversation, Long> implements AiConversationService {

    /**
     * Get the conversation object based on the chat message.
     * If there is no conversation ID, create a new conversation object.
     *
     * @param chatMessage Chat message
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void handleConversation(ChatMessage chatMessage) {
        if (chatMessage.getConversationId() == null) {
            AiConversation conversation = new AiConversation();
            String content = chatMessage.getContent();
            conversation.setTitle(content.length() > 10 ? content.substring(0, 10) : content);
            conversation.setRobotId(chatMessage.getRobotId());
            Long conversationId = this.createOne(conversation);
            // Update the conversation ID to the chat message
            chatMessage.setConversationId(conversationId);
        } else {
            // Check if the conversation ID exists
            Filters filters = Filters.eq(ModelConstant.ID, chatMessage.getConversationId());
            AiConversation conversation = this.searchOne(filters);
            Assert.notNull(conversation, "Conversation ID not exists: {0}", chatMessage.getConversationId());
            Assert.isTrue(Objects.equals(chatMessage.getRobotId(), conversation.getRobotId()),
                    "Chat message robot ID does not match the conversation robot ID: {0}", chatMessage);
        }
    }

}