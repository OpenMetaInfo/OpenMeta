package info.openmeta.starter.designer.entity;

import com.fasterxml.jackson.databind.JsonNode;
import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.framework.orm.enums.ViewType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * DesignView Model
 */
@Data
@Schema(name = "DesignView")
@EqualsAndHashCode(callSuper = true)
public class DesignView extends BaseModel {

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

    @Schema(description = "Model Name")
    private String modelName;

    @Schema(description = "View Name")
    private String name;

    @Schema(description = "View Code")
    private String code;

    @Schema(description = "View Type")
    private ViewType type;

    @Schema(description = "Sequence")
    private Integer sequence;

    @Schema(description = "Structure")
    private JsonNode structure;

    @Schema(description = "Default Filters")
    private JsonNode defaultFilter;

    @Schema(description = "Default Order")
    private JsonNode defaultOrder;

    @Schema(description = "Navigation ID")
    private Long navId;

    @Schema(description = "Public View")
    private Boolean publicView;

    @Schema(description = "Default View")
    private Boolean defaultView;

    @Schema(description = "Disabled")
    private Boolean disabled;
}