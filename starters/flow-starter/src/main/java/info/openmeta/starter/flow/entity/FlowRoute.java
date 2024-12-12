package info.openmeta.starter.flow.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * FlowRoute Model
 */
@Data
@Schema(name = "FlowRoute")
@EqualsAndHashCode(callSuper = true)
public class FlowRoute extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    protected Long id;

    @Schema(description = "Model Name")
    private String model;

    @Schema(description = "Previous Node ID")
    private Long previousNodeId;

    @Schema(description = "Next Node ID")
    private Long nextNodeId;

    @Schema(description = "Sequence")
    private Integer sequence;

    @Schema(description = "Disabled")
    private Boolean disabled;
}