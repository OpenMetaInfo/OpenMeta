package info.openmeta.starter.flow.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Flow Node Type
 */
@Getter
@AllArgsConstructor
public enum FlowNodeType {
    DATA_PROCESS("DataProcess", "Data Process"),
    LOOP_BY_DATASET("LoopByDataset", "Loop By Dataset"),
    LOOP_BY_PAGE("LoopByPage", "Loop By Page"),
    APPROVAL_NODE("ApprovalNode", "Approval Node"),
    STAGE_NODE("StageNode", "Stage Node");

    @JsonValue
    private final String type;
    private final String name;
}
