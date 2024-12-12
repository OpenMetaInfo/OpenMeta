package info.openmeta.starter.flow.entity;

import com.fasterxml.jackson.databind.JsonNode;
import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.starter.flow.enums.FlowActionType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * FlowAction Model
 */
@Data
@Schema(name = "FlowAction")
@EqualsAndHashCode(callSuper = true)
public class FlowAction extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Action Name")
    private String name;

    @Schema(description = "Action Code")
    private String code;

    @Schema(description = "Flow ID")
    private Long flowId;

    @Schema(description = "Node ID")
    private Long nodeId;

    @Schema(description = "Sequence")
    private Integer sequence;

    @Schema(description = "Action Type")
    private FlowActionType actionType;

    @Schema(description = "Action Execute Condition")
    private String actionCondition;

    @Schema(description = "Action Params")
    private JsonNode actionParams;

    @Schema(description = "Exception Policy")
    private JsonNode exceptionPolicy;

    @Schema(description = "Disabled")
    private Boolean disabled;
}