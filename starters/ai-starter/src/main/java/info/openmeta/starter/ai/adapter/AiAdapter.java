package info.openmeta.starter.ai.adapter;

import com.plexpt.chatgpt.entity.chat.ChatCompletion;
import com.plexpt.chatgpt.entity.chat.ChatCompletionResponse;
import info.openmeta.starter.ai.listener.StreamResponseListener;

/**
 * AI Adapter Interface
 */
public interface AiAdapter {

    /**
     * Stream Response Robot Chat
     * @param chatCompletion Chat request
     * @param listener Stream response listener
     */
    void streamChat(ChatCompletion chatCompletion, StreamResponseListener listener);

    /**
     * Robot Chat
     * @param chatCompletion Chat request
     */
    ChatCompletionResponse chat(ChatCompletion chatCompletion);

}
