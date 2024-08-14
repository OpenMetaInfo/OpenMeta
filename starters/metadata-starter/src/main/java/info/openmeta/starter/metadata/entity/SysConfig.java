package info.openmeta.starter.metadata.entity;

import com.fasterxml.jackson.databind.JsonNode;
import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * SysConfig Model
 */
@Data
@Schema(name = "SysConfig")
@EqualsAndHashCode(callSuper = true)
public class SysConfig extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "App ID")
    private Long appId;

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

    @Schema(description = "Disabled")
    private Boolean disabled;
}