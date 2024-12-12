package info.openmeta.starter.ai.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * AiFeedback Model
 */
@Data
@Schema(name = "AiFeedback")
@EqualsAndHashCode(callSuper = true)
public class AiFeedback extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    protected Long id;

    @Schema(description = "Conversation ID")
    private Long conversationId;

    @Schema(description = "Message ID")
    private Long messageId;

    @Schema(description = "Feedback Content")
    private String feedback;

    @Schema(description = "Disabled")
    private Boolean disabled;
}