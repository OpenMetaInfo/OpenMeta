package info.openmeta.starter.flow.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Flow Status
 */
@Getter
@AllArgsConstructor
public enum FlowStatus {
    INITIAL("Initial"),
    RUNNING("Running"),
    APPROVING("Approving"),
    FAILED("Failed"),
    COMPLETED("Completed");

    @JsonValue
    private final String type;
}
