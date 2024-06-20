package info.openmeta.framework.orm.entity;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * Abstract class of base model, with id and audit fields.
 */
@Data
@Schema(name = "BaseModel")
public abstract class BaseModel implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Creation time")
    protected LocalDateTime createdTime;

    @Schema(description = "Creator ID")
    protected Long createdId;

    @Schema(description = "Created By")
    protected String createdBy;

    @Schema(description = "Update time")
    protected LocalDateTime updatedTime;

    @Schema(description = "Updater ID")
    protected Long updatedId;

    @Schema(description = "Updated By")
    protected String updatedBy;
}
