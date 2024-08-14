package info.openmeta.starter.flow.message.dto;

import info.openmeta.framework.base.context.Context;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.Map;

/**
 * Flow event message
 */
@Data
public class FlowEventMessage {

    private Long flowId;
    private Long flowNodeId;
    private String flowModel;
    private Boolean rollbackOnFail;
    private Long triggerId;
    private String triggeredModel;
    private Long triggerRowId;
    private Map<String, Object> triggerParams;

    private LocalDateTime eventTime;
    private Context context;

    public FlowEventMessage() {
        this.eventTime = LocalDateTime.now();
    }
}
