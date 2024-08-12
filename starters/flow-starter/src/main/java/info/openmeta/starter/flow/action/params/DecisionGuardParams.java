package info.openmeta.starter.flow.action.params;

import info.openmeta.starter.flow.enums.ActionExceptionSignal;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Parameters for decision guard.
 * Determines if the flow can continue based on the condition.
 * If true, the flow continues; if false, an exception signal is thrown.
 * Example:
 * <p>
 * {
 *     "passCondition": "GetIds != nil",
 *     "exceptionSignal": "EndFlow",
 *     "exceptionMessage": "The ID collection is empty, flow terminated!"
 * }
 * </p>
 */
@Schema(name = "Decision Guard Params")
@Data
@NoArgsConstructor
public class DecisionGuardParams implements ActionParams {

    @Schema(description = "The pass condition expression, return boolean value")
    private String passCondition;

    @Schema(description = "The exception signal when the pass condition is false")
    private ActionExceptionSignal exceptionSignal;

    @Schema(description = "Exception message, support string interpolation, #{var}")
    private String exceptionMessage;
}
