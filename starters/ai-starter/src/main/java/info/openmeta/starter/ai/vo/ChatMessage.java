package info.openmeta.starter.ai.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * AI Chat Message
 */
@Data
@Schema(name = "AI Chat Message")
@NoArgsConstructor
@AllArgsConstructor
public class ChatMessage {

    @Schema(description = "Robot ID")
    @NotNull(message = "Robot ID cannot be null!")
    private Long robotId;

    @Schema(description = "Conversation ID")
    private Long conversationId;

    @Schema(description = "Chat Content")
    @NotBlank(message = "Chat content cannot be empty!")
    private String content;

}
