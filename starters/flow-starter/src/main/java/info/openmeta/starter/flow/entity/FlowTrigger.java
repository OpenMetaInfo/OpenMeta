package info.openmeta.starter.flow.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.starter.flow.enums.TriggerEventType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.util.List;

/**
 * FlowTrigger Model
 */
@Data
@Schema(name = "FlowTrigger")
@EqualsAndHashCode(callSuper = true)
public class FlowTrigger extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Trigger Name")
    private String name;

    @Schema(description = "Trigger Code")
    private String triggerCode;

    @Schema(description = "Trigger Event Type")
    private TriggerEventType eventType;

    @Schema(description = "Triggered Model")
    private String triggeredModel;

    @Schema(description = "Triggered Fields")
    private List<String> triggeredFields;

    @Schema(description = "Trigger Condition")
    private String triggerCondition;

    @Schema(description = "Cron Job ID")
    private Long cronId;

    @Schema(description = "Triggered Flow")
    private Long flowId;

    @Schema(description = "Disabled")
    private Boolean disabled;
}