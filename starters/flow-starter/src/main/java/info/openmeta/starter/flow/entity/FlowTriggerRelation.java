package info.openmeta.starter.flow.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * FlowTriggerRelation Model
 */
@Data
@Schema(name = "FlowTriggerRelation")
@EqualsAndHashCode(callSuper = true)
public class FlowTriggerRelation extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Flow ID")
    private Long flowId;

    @Schema(description = "Trigger ID")
    private Long triggerId;

    @Schema(description = "Disabled")
    private Boolean disabled;
}