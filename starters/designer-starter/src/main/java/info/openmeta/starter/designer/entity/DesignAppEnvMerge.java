package info.openmeta.starter.designer.entity;

import com.fasterxml.jackson.databind.JsonNode;
import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * DesignAppEnvMerge Model
 */
@Data
@Schema(name = "DesignAppEnvMerge")
@EqualsAndHashCode(callSuper = true)
public class DesignAppEnvMerge extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "App ID")
    private Long appId;

    @Schema(description = "Source Env ID")
    private Long sourceEnvId;

    @Schema(description = "Target Env ID")
    private Long targetEnvId;

    @Schema(description = "Merge description")
    private String description;

    @Schema(description = "Merge Content")
    private JsonNode mergeContent;

    @Schema(description = "Deleted")
    private Boolean deleted;
}