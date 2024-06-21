package info.openmeta.framework.web.task.params;

import lombok.Data;

import java.io.Serializable;
import java.util.Set;

/**
 */
@Data
public class RecomputeHandlerParams implements TaskHandlerParams {
    private String model;
    private Set<String> fields;
    private Set<Serializable> ids;
}
