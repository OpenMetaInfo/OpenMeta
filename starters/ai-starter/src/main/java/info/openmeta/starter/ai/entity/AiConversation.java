package info.openmeta.starter.ai.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * AiConversation Model
 */
@Data
@Schema(name = "AiConversation")
@EqualsAndHashCode(callSuper = true)
public class AiConversation extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Conversation Title")
    private String title;

    @Schema(description = "Robot ID")
    private Long robotId;

    @Schema(description = "Total Tokens")
    private Integer totalTokens;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Disabled")
    private Boolean disabled;
}