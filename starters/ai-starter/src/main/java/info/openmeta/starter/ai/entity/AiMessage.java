package info.openmeta.starter.ai.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * AiMessage Model
 */
@Data
@Schema(name = "AiMessage")
@EqualsAndHashCode(callSuper = true)
public class AiMessage extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Conversation ID")
    private Long conversationId;

    @Schema(description = "Query Content")
    private String query;

    @Schema(description = "Answer Content")
    private String answer;

    @Schema(description = "Input Tokens")
    private Integer inputTokens;

    @Schema(description = "Output Tokens")
    private Integer outputTokens;

    @Schema(description = "Total Tokens")
    private Integer totalTokens;

}