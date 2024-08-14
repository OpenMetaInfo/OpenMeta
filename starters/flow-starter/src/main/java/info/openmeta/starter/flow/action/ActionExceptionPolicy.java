package info.openmeta.starter.flow.action;

import info.openmeta.starter.flow.enums.ActionExceptionSignal;
import info.openmeta.starter.flow.enums.ActionExceptionType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * Action Exception Policy
 * Mainly for the GetData and ComputeData actions to handle the return value exception.
 */
@Data
@Schema(name = "Action Exception Policy")
public class ActionExceptionPolicy {

    @Schema(description = "Exception Type")
    private ActionExceptionType exceptionWhen;

    @Schema(description = "Exception Signal")
    private ActionExceptionSignal exceptionSignal;

    @Schema(description = "Exception message, supports string interpolation, `#{var}`")
    private String exceptionMessage;

}
