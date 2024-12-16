package info.openmeta.starter.designer.entity;

import com.fasterxml.jackson.databind.JsonNode;
import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.starter.designer.enums.PublishStatus;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * DesignAppVersionPublished Model
 */
@Data
@Schema(name = "DesignAppVersionPublished")
@EqualsAndHashCode(callSuper = true)
public class DesignAppVersionPublished extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "App ID")
    private Long appId;

    @Schema(description = "Env ID")
    private Long envId;

    @Schema(description = "Version ID")
    private Long versionId;

    @Schema(description = "Publish Status")
    private PublishStatus publishStatus;

    @Schema(description = "Publish Duration (S)")
    private Double publishDuration;

    @Schema(description = "Publish Content")
    private JsonNode publishContent;

    @Schema(description = "Disabled")
    private Boolean disabled;
}