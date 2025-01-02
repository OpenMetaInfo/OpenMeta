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
    VALIDATE_DATA("ValidateData", "Validate Data"),
    GET_DATA("GetData", "Get Data"),
    COMPUTE_DATA("ComputeData", "Compute Data"),
    UPDATE_DATA("UpdateData", "Update Data"),
    CREATE_DATA("CreateData", "Create Data"),
    DELETE_DATA("DeleteData", "Delete Data"),
    BRANCH_GATEWAY("BranchGateway", "Branch Gateway"),
    CONDITION("Condition", "Condition"),
    ASYNC_TASK("AsyncTask", "Async Task"),
    TRIGGER_SUBFLOW("TriggerSubflow", "Trigger Subflow"),
    EXTRACT_TRANSFORM("ExtractTransform", "Extract Transform"),
    QUERY_AI("QueryAi", "Query AI"),
    TRANSFER_STAGE("TransferStage", "Transfer Stage"),
    GENERATE_REPORT("GenerateReport", "Generate Report"),
    SEND_MESSAGE("SendMessage", "Send Message"),
    WEB_HOOK("WebHook", "WebHook"),
    RETURN_DATA("ReturnData", "Return Data"),
    LOOP_BY_DATASET("LoopByDataset", "Loop By Dataset"),
    LOOP_BY_PAGE("LoopByPage", "Loop By Page"),
    APPROVAL_NODE("ApprovalNode", "Approval Node"),
    ;

    @JsonValue
    private final String type;
    private final String name;
}
