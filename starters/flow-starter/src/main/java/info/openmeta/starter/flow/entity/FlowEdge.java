package info.openmeta.starter.flow.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * FlowEdge Model
 */
@Data
@Schema(name = "FlowEdge")
@EqualsAndHashCode(callSuper = true)
public class FlowEdge extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Edge Label")
    private String label;

    @Schema(description = "Source Node ID")
    private Long sourceId;

    @Schema(description = "Target Node ID")
    private Long targetId;
}