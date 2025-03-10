package info.openmeta.starter.designer.entity;

import com.fasterxml.jackson.databind.JsonNode;
import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * DesignConfig Model
 */
@Data
@Schema(name = "DesignConfig")
@EqualsAndHashCode(callSuper = true)
public class DesignConfig extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "External ID")
    private Long externalId;

    @Schema(description = "App ID")
    private Long appId;

    @Schema(description = "Env ID")
    private Long envId;

    @Schema(description = "Name")
    private String name;

    @Schema(description = "Code")
    private String code;

    @Schema(description = "Value")
    private JsonNode value;

    @Schema(description = "Value Data Type")
    private String valueType;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Deleted")
    private Boolean deleted;
}