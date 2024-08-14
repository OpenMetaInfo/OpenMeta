package info.openmeta.starter.cron.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Cron status
 */
@Getter
@AllArgsConstructor
public enum CronStatus {
    Scheduled("Scheduled", "Scheduled for a specified time."),
    RUNNING("Running", "Currently executing."),
    COMPLETED("Completed", "Finished successfully."),
    PAUSED("Paused", "Temporarily paused."),
    CANCELLED("Cancelled", "Cancelled before completion."),
    SKIPPED("Skipped", "Skipped due to unmet execution conditions."),
    TIMEOUT("Timeout", "Interrupted by execution timeout."),
    FAILED("Failed", "Execution failed.");

    @JsonValue
    private final String status;

    private final String description;
}
