package info.openmeta.starter.flow.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * FlowApproval Model
 */
@Data
@Schema(name = "FlowApproval")
@EqualsAndHashCode(callSuper = true)
public class FlowApproval extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Approval Node Name")
    private String name;

    @Schema(description = "Model Name")
    private String model;

    @Schema(description = "Flow Node ID")
    private Long flowNodeId;

    @Schema(description = "Approval Type")
    private String approvalType;

    @Schema(description = "Disabled")
    private Boolean disabled;
}