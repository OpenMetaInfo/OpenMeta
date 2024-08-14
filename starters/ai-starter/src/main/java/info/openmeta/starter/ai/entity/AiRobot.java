package info.openmeta.starter.ai.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.starter.ai.enums.AiModelProvider;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * AiRobot Model
 */
@Data
@Schema(name = "AiRobot")
@EqualsAndHashCode(callSuper = true)
public class AiRobot extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Robot Name")
    private String name;

    @Schema(description = "Robot Code")
    private String code;

    @Schema(description = "AI Model ID")
    private Long aiModelId;

    @Schema(description = "AI Model Code")
    private String aiModel;

    @Schema(description = "AI Provider")
    private AiModelProvider aiProvider;

    @Schema(description = "system_prompt")
    private String systemPrompt;

    @Schema(description = "Model Max Context Tokens")
    private Integer modelMaxTokens;

    @Schema(description = "Input Tokens Limit")
    private Integer inputTokensLimit;

    @Schema(description = "Output Tokens Limit")
    private Integer outputTokensLimit;

    @Schema(description = "Temperature")
    private Double temperature;

    @Schema(description = "Enable Stream Output")
    private Boolean stream;

    @Schema(description = "Presence Penalty")
    private Double presencePenalty;

    @Schema(description = "Frequency Penalty")
    private Double frequencyPenalty;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Disabled")
    private Boolean disabled;
}