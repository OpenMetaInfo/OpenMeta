package info.openmeta.starter.flow.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.starter.flow.enums.FlowStatus;
import info.openmeta.starter.flow.enums.FlowType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * FlowInstance Model
 */
@Data
@Schema(name = "FlowInstance")
@EqualsAndHashCode(callSuper = true)
public class FlowInstance extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private String id;

    @Schema(description = "Main Model")
    private String modelName;

    @Schema(description = "Row Data ID")
    private String rowId;

    @Schema(description = "Flow ID")
    private String flowId;

    @Schema(description = "Flow Type")
    private FlowType flowType;

    @Schema(description = "Trigger ID")
    private String triggerId;

    @Schema(description = "Current Node ID")
    private String currentNodeId;

    @Schema(description = "Current Status")
    private FlowStatus currentStatus;
}