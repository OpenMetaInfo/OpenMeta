package info.openmeta.starter.designer.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * DesignAppVersionVO for creating a new version
 */
@Data
@Schema(name = "DesignAppVersionVO", description = "DesignAppVersionVO for creating a new version")
public class DesignAppVersionVO {

    @Schema(description = "App ID")
    private Long appId;

    @Schema(description = "Env ID")
    private Long envId;

    @Schema(description = "Version Name")
    private String name;

    @Schema(description = "Upgrade description")
    private String description;

}