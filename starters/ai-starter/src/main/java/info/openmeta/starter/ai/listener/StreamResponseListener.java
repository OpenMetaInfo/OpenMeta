package info.openmeta.starter.ai.listener;

import com.plexpt.chatgpt.entity.chat.ChatChoice;
import com.plexpt.chatgpt.entity.chat.ChatCompletionResponse;
import info.openmeta.framework.base.exception.IntegrationException;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.starter.ai.constant.AiConstant;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import okhttp3.Response;
import okhttp3.sse.EventSource;
import okhttp3.sse.EventSourceListener;
import org.jetbrains.annotations.NotNull;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.util.List;
import java.util.function.Consumer;

/**
 * Stream Response Listener
 */
@Slf4j
@RequiredArgsConstructor
public class StreamResponseListener extends EventSourceListener {

    final SseEmitter sseEmitter;

    // Merge streamed message for passing to the callback function for persistent processing
    protected StringBuffer mergedMessage = new StringBuffer();

    // callback function
    @Setter
    @Getter
    protected Consumer<String> callback = s -> {};

    @Override
    public void onOpen(@NotNull EventSource eventSource, @NotNull Response response) {
        // do nothing
    }

    @Override
    public void onClosed(@NotNull EventSource eventSource) {
        // do nothing
    }

    /**
     * Stream response event
     * @param eventSource EventSource
     * @param id Id
     * @param type Type
     * @param data Data
     */
    @Override
    public void onEvent(@NotNull EventSource eventSource, String id, String type, String data) {
        if (data.equals(AiConstant.STREAM_END_MESSAGE)) {
            // Finish the stream and send the merged message to the listener callback method.
            callback.accept(mergedMessage.toString());
            // Release eventSource resource
            eventSource.cancel();
            return;
        }
        // Parse Json data to ChatCompletionResponse
        ChatCompletionResponse response = JsonMapper.stringToObject(data, ChatCompletionResponse.class);
        List<ChatChoice> chatChoices = response.getChoices();
        if (chatChoices == null || chatChoices.isEmpty()) {
            return;
        }
        // Extract the message content from choices[0].delta.content and send it to the frontend.
        String content = chatChoices.getFirst().getDelta().getContent();
        if (content != null) {
            mergedMessage.append(content);
            try {
                sseEmitter.send(content);
            } catch (IOException e) {
                throw new IntegrationException(e.getMessage(), e);
            }
        }
    }

    /**
     * Failure
     * @param eventSource EventSource
     * @param throwable Throwable
     * @param response Response
     */
    @Override
    public void onFailure(@NotNull EventSource eventSource, Throwable throwable, Response response) {
        try {
            log.error("Stream connection or response exception!", throwable);
            if (response != null && response.body() != null) {
                log.error("response body: {}", response.body());
            }
            // Close sseEmitter
            sseEmitter.complete();
        } catch (Exception e) {
            log.error("Exception in SSE failure handling!", e);
        } finally {
            // Release eventSource resource
            eventSource.cancel();
        }
    }
}
