package info.openmeta.starter.designer.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.starter.designer.enums.AppEnvType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.time.LocalDateTime;

/**
 * DesignAppEnv Model
 */
@Data
@Schema(name = "DesignAppEnv")
@EqualsAndHashCode(callSuper = true)
public class DesignAppEnv extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Env Name")
    private String name;

    @Schema(description = "App ID")
    private Long appId;

    @Schema(description = "App Code")
    private String appCode;

    @Schema(description = "Env Type")
    private AppEnvType envType;

    @Schema(description = "Upgrade API EndPoint")
    private String upgradeEndpoint;

    @Schema(description = "Last Publish Time")
    private LocalDateTime lastPublishTime;

    @Schema(description = "Client ID")
    private String clientId;

    @Schema(description = "Client Secret")
    private String clientSecret;

    @Schema(description = "Async Upgrade")
    private Boolean asyncUpgrade;

    @Schema(description = "Auto Upgrade")
    private Boolean autoUpgrade;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Disabled")
    private Boolean disabled;
}