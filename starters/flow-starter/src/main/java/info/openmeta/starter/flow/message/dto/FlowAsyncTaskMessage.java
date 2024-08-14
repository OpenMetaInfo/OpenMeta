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

    private Long flowId;
    private Long nodeId;
    private Long actionId;
    private String asyncTaskCode;
    private Map<String, Object> asyncTaskParams;

    private Context context;
}
