package info.openmeta.starter.flow.message.dto;

import info.openmeta.framework.base.context.Context;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * Flow async task message
 */
@Data
@NoArgsConstructor
public class FlowAsyncTaskMessage {

    private String flowId;
    private String nodeId;
    private String asyncTaskHandlerCode;
    private Map<String, Object> asyncTaskParams;

    private Context context;
}
