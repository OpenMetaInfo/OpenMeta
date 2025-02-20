package info.openmeta.starter.flow.vo;

import lombok.Data;

import java.io.Serializable;
import java.util.Map;

/**
 * Flow event VO for simulation
 */
@Data
public class FlowEventVO {
    private String flowId;
    private String flowNodeId;
    private Boolean rollbackOnFail;
    private String triggerId;
    private String sourceModel;
    private Serializable triggerRowId;
    private Map<String, Object> triggerParams;
}
