package info.openmeta.starter.flow.message.dto;

import info.openmeta.framework.base.context.Context;
import lombok.Data;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Map;

/**
 * Flow event message
 */
@Data
public class FlowEventMessage {

    private String flowId;
    private String flowNodeId;
    private Boolean sync;
    private Boolean rollbackOnFail;
    private String triggerId;
    private String sourceModel;
    private Serializable sourceRowId;
    private Map<String, Object> triggerParams;

    private LocalDateTime eventTime;
    private Context context;

    public FlowEventMessage() {
        this.eventTime = LocalDateTime.now();
    }
}
