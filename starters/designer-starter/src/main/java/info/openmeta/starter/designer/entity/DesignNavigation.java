package info.openmeta.starter.designer.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * DesignNavigation Model
 */
@Data
@Schema(name = "DesignNavigation")
@EqualsAndHashCode(callSuper = true)
public class DesignNavigation extends BaseModel {

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

    @Schema(description = "Type")
    private String type;

    @Schema(description = "Code")
    private String code;

    @Schema(description = "Model Name")
    private String modelName;

    @Schema(description = "Parent Navigation")
    private Long parentId;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Default filters")
    private String filter;

    @Schema(description = "Disabled")
    private Boolean disabled;
}