package info.openmeta.starter.flow.entity;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.starter.flow.enums.FlowType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.util.List;

/**
 * FlowConfig Model
 */
@Data
@Schema(name = "FlowConfig")
@EqualsAndHashCode(callSuper = true)
public class FlowConfig extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Flow Name")
    private String name;

    @Schema(description = "Model Name")
    private String model;

    @Schema(description = "Flow Type")
    private FlowType flowType;

    @Schema(description = "Flow Trigger")
    private Long triggerId;

    @Schema(description = "Node List")
    private List<FlowNode> nodeList;

    @Schema(description = "Is Sync Executed Flow")
    private Boolean sync;

    @Schema(description = "Enable Debug Model")
    private Boolean debugMode;

    @Schema(description = "Rollback On Fail")
    private Boolean rollbackOnFail;

    @Schema(description = "Readonly Flow")
    private Boolean readonly;

    @Schema(description = "Version")
    private String version;

    @Schema(description = "Flow Description")
    private String description;

    @Schema(description = "Data Scope")
    private Filters dataScope;

    @Schema(description = "Disabled")
    private Boolean disabled;
}