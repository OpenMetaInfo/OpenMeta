package info.openmeta.starter.ai.util;

import com.knuddels.jtokkit.Encodings;
import com.knuddels.jtokkit.api.Encoding;
import com.knuddels.jtokkit.api.EncodingRegistry;
import info.openmeta.framework.base.utils.Assert;
import lombok.extern.slf4j.Slf4j;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

/**
 * GPT Token Utils
 */
@Slf4j
public class TokenUtils {

    // Encoding Registry for AI Models
    private static final EncodingRegistry encodingRegistry = Encodings.newLazyEncodingRegistry();

    /**
     * Count tokens for the specified aiModel
     *
     * @param aiModel AI model name
     * @param content Text content
     * @return Number of text tokens
     */
    public static int count(String aiModel, String content) {
        Optional<Encoding> encoding = encodingRegistry.getEncodingForModel(aiModel);
        return encoding.map(e -> e.countTokens(content)).orElseGet(() -> {
            log.error("Counting tokens for the {} model is not supported!", aiModel);
            return 0;
        });
    }

    /**
     * Count tokens for the specified aiModel
     *
     * @param aiModel AI model name
     * @param contents content list
     * @return total number of text tokens
     */
    public static int count(String aiModel, List<String> contents) {
        Assert.allNotNull(contents, "AI content cannot be null!");
        return contents.stream().mapToInt(content -> count(aiModel, content)).sum();
    }

    /**
     * Count tokens for the specified aiModel
     *
     * @param aiModel AI model name
     * @param contents content array
     * @return total number of text tokens
     */
    public static int count(String aiModel, String... contents) {
        return count(aiModel, Arrays.asList(contents));
    }
}
