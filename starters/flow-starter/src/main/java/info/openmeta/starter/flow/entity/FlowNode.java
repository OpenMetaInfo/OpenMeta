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
    private String id;

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

    @Schema(description = "Parent Node ID")
    private String parentId;

    @Schema(description = "Child Nodes")
    private List<FlowNode> childNodes;

    @Schema(description = "Node Execute Condition")
    private String nodeCondition;

    @Schema(description = "Node Params")
    private JsonNode nodeParams;

    @Schema(description = "Exception Policy")
    private JsonNode exceptionPolicy;

    @Schema(description = "Position")
    private JsonNode position;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Disabled")
    private Boolean disabled;
}