package info.openmeta.starter.flow.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * FlowStage Model
 */
@Data
@Schema(name = "FlowStage")
@EqualsAndHashCode(callSuper = true)
public class FlowStage extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Model Name")
    private String modelName;

    @Schema(description = "Flow ID")
    private Long flowId;

    @Schema(description = "Stage Name")
    private String name;

    @Schema(description = "Stage Description")
    private String description;

    @Schema(description = "Disabled")
    private Boolean disabled;
}