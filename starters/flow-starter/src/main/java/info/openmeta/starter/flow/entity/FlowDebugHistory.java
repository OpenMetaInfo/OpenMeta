package info.openmeta.starter.flow.entity;

import com.fasterxml.jackson.databind.JsonNode;
import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.starter.flow.enums.FlowStatus;
import info.openmeta.starter.flow.enums.FlowType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.time.LocalDateTime;

/**
 * FlowDebugHistory Model
 */
@Data
@Schema(name = "FlowDebugHistory")
@EqualsAndHashCode(callSuper = true)
public class FlowDebugHistory extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Model Name")
    private String model;

    @Schema(description = "Flow ID")
    private Long flowId;

    @Schema(description = "Flow Type")
    private FlowType flowType;

    @Schema(description = "Flow Status")
    private FlowStatus flowStatus;

    @Schema(description = "Main Flow ID")
    private Long mainFlowId;

    @Schema(description = "Parent Flow ID")
    private Long parentFlowId;

    @Schema(description = "Start Time")
    private LocalDateTime startTime;

    @Schema(description = "End Time")
    private LocalDateTime endTime;

    @Schema(description = "Event Message")
    private JsonNode eventMessage;

    @Schema(description = "Action Trace")
    private JsonNode actionTrace;
}