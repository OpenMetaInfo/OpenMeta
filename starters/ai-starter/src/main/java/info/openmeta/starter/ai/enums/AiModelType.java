package info.openmeta.starter.ai.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * AI Model Type
 */
@Getter
@AllArgsConstructor
public enum AiModelType {
    GPT("GPT", "GPT"),
    IMAGE("Image", "Image Model"),
    AUDIO("Audio", "Audio Model"),
    VIDEO("Video", "Video Model");

    @JsonValue
    private final String type;

    private final String name;
}
