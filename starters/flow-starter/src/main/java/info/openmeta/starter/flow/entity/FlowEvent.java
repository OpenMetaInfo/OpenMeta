package info.openmeta.starter.flow.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.starter.flow.enums.TriggerEventType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * FlowEvent Model
 */
@Data
@Schema(name = "FlowEvent")
@EqualsAndHashCode(callSuper = true)
public class FlowEvent extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Flow ID")
    private Long flowId;

    @Schema(description = "Flow Node ID")
    private Long flowNodeId;

    @Schema(description = "Flow Main Model")
    private String flowModel;

    @Schema(description = "Row Data ID")
    private String rowId;

    @Schema(description = "Trigger ID")
    private Long triggerId;

    @Schema(description = "Trigger Type")
    private TriggerEventType triggerType;

    @Schema(description = "Triggered Model")
    private String triggeredModel;

    @Schema(description = "Disabled")
    private Boolean disabled;
}