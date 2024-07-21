package info.openmeta.starter.ai.service.impl;

import com.plexpt.chatgpt.entity.billing.Usage;
import com.plexpt.chatgpt.entity.chat.ChatCompletion;
import com.plexpt.chatgpt.entity.chat.ChatCompletionResponse;
import com.plexpt.chatgpt.entity.chat.Message;
import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.ai.adapter.AiAdapter;
import info.openmeta.starter.ai.adapter.AiAdapterFactory;
import info.openmeta.starter.ai.entity.AiMessage;
import info.openmeta.starter.ai.entity.AiRobot;
import info.openmeta.starter.ai.listener.StreamResponseListener;
import info.openmeta.starter.ai.service.AiConversationService;
import info.openmeta.starter.ai.service.AiMessageService;
import info.openmeta.starter.ai.service.AiRobotService;
import info.openmeta.starter.ai.util.TokenUtils;
import info.openmeta.starter.ai.vo.ChatMessage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.Arrays;

/**
 * AiRobot Model Service Implementation
 */
@Service
public class AiRobotServiceImpl extends EntityServiceImpl<AiRobot, Long> implements AiRobotService {

    @Value("${ai.response.timeout:60000}")
    private Long timeout;

    @Autowired
    private AiAdapterFactory aiAdapterFactory;

    @Autowired
    private AiConversationService conversationService;

    @Autowired
    private AiMessageService aiMessageService;

    /**
     * Get robot object based on chat message
     * @param chatMessage Chat message
     * @return Robot object
     */
    private AiRobot getAiRobot(ChatMessage chatMessage) {
        Filters robotFilters = Filters.eq(ModelConstant.ID, chatMessage.getRobotId());
        AiRobot aiRobot = this.searchOne(robotFilters);
        Assert.notNull(aiRobot, "Robot ID not exist: {0}", chatMessage.getRobotId());
        return aiRobot;
    }

    /**
     * Build ChatCompletion object
     *
     * @param aiRobot Robot object
     * @param chatMessage ChatMessage object
     * @return ChatCompletion
     */
    private ChatCompletion buildChatCompletion(AiRobot aiRobot, ChatMessage chatMessage) {
        conversationService.handleConversation(chatMessage);
        // [System message, User message]
        Message systemMessage = Message.ofSystem(aiRobot.getSystemPrompt());
        Message userMessage = Message.of(chatMessage.getContent());
        int messageTokens = TokenUtils.count(aiRobot.getAiModel(), systemMessage.getContent(), userMessage.getContent());
        Assert.isTrue(messageTokens <= aiRobot.getInputTokensLimit(),
                "The input tokens exceeds the maximum limit configured for the robot: {0} > {1}",
                messageTokens, aiRobot.getInputTokensLimit());
        return ChatCompletion.builder()
                .model(aiRobot.getAiModel())
                .messages(Arrays.asList(systemMessage, userMessage))
                .maxTokens(aiRobot.getOutputTokensLimit())
                .temperature(aiRobot.getTemperature())
                .build();
    }

    /**
     * Count token usage based on message content
     *
     * @param aiModel Model name
     * @param query Query content
     * @param answer Answer content
     * @return Usage object
     */
    private Usage countUsage(String aiModel, String query, String answer) {
        Usage usage = new Usage();
        usage.setPromptTokens(TokenUtils.count(aiModel, query));
        usage.setCompletionTokens(TokenUtils.count(aiModel, answer));
        usage.setTotalTokens(usage.getPromptTokens() + usage.getCompletionTokens());
        return usage;
    }

    /**
     * Stream Chat
     *
     * @param chatMessage Chat message
     * @return SseEmitter
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public SseEmitter streamChat(ChatMessage chatMessage) {
        Context context = ContextHolder.getContext();
        AiRobot aiRobot = this.getAiRobot(chatMessage);
        ChatCompletion chatCompletion = this.buildChatCompletion(aiRobot, chatMessage);
        AiAdapter aiAdapter = aiAdapterFactory.getAiAdapter(aiRobot.getAiProvider());

        // Use SseEmitter to send messages to the client
        SseEmitter sseEmitter = new SseEmitter(timeout);
        // Close sseEmitter on timeout
        sseEmitter.onTimeout(sseEmitter::complete);
        // Close sseEmitter on error
        sseEmitter.onError(throwable -> sseEmitter.complete());

        // Listen for LLM response through SseStreamListener
        StreamResponseListener responseListener = new StreamResponseListener(sseEmitter);
        // Set the callback function to persist the chat message after the chat is completed
        responseListener.setOnComplete(answer -> {
            // Close the SSE connection to the client
            sseEmitter.complete();
            // Count the tokens and save the chat message
            Usage usage = this.countUsage(chatCompletion.getModel(), chatMessage.getContent(), answer);
            ContextHolder.setContext(context);
            aiMessageService.saveAiMessage(chatMessage, answer, usage);
        });
        // Send the chat message
        aiAdapter.streamChat(chatCompletion, responseListener);
        return sseEmitter;
    }

    /**
     * Chat API
     *
     * @param chatMessage Chat message
     * @return AiMessage
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public AiMessage chat(ChatMessage chatMessage) {
        AiRobot aiRobot = this.getAiRobot(chatMessage);
        ChatCompletion chatCompletion = this.buildChatCompletion(aiRobot, chatMessage);
        AiAdapter aiAdapter = aiAdapterFactory.getAiAdapter(aiRobot.getAiProvider());

        // Send the chat message
        ChatCompletionResponse chatResponse = aiAdapter.chat(chatCompletion);
        // Save the chat message
        String answer = chatResponse.getChoices().getFirst().getMessage().getContent();
        return aiMessageService.saveAiMessage(chatMessage, answer, chatResponse.getUsage());
    }
}