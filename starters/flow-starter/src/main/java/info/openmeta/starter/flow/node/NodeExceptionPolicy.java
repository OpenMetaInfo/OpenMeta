package info.openmeta.starter.flow.node;

import info.openmeta.starter.flow.enums.NodeExceptionSignal;
import info.openmeta.starter.flow.enums.NodeExceptionType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * Node Exception Policy
 * Mainly for the GetData and ComputeData nodes to handle the return value exception.
 */
@Data
@Schema(name = "Node Exception Policy")
public class NodeExceptionPolicy {

    @Schema(description = "Exception Type")
    private NodeExceptionType exceptionWhen;

    @Schema(description = "Exception Signal")
    private NodeExceptionSignal exceptionSignal;

    @Schema(description = "Exception message, supports string interpolation, `#{var}`")
    private String exceptionMessage;

}
