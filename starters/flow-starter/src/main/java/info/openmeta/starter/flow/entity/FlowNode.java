package info.openmeta.starter.flow.entity;

import com.fasterxml.jackson.databind.JsonNode;
import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.starter.flow.enums.FlowNodeType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.util.List;

/**
 * FlowNode Model
 */
@Data
@Schema(name = "FlowNode")
@EqualsAndHashCode(callSuper = true)
public class FlowNode extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    protected Long id;

    @Schema(description = "Node Name")
    private String name;

    @Schema(description = "Flow ID")
    private Long flowId;

    @Schema(description = "Stage ID")
    private Long stageId;

    @Schema(description = "Node Type")
    private FlowNodeType nodeType;

    @Schema(description = "Sequence")
    private Integer sequence;

    @Schema(description = "Node Execute Condition")
    private String nodeCondition;

    @Schema(description = "Loop Params")
    private JsonNode loopParams;

    @Schema(description = "Action List")
    private List<FlowAction> actionList;

    @Schema(description = "Disabled")
    private Boolean disabled;
}