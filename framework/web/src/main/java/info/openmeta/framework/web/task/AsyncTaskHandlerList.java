package info.openmeta.framework.web.task;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Enums of asynchronous task handlers: code to name mapping.
 */
@Getter
@AllArgsConstructor
public enum AsyncTaskHandlerList {
    RECOMPUTE_MODEL_FIELD("RecomputeModelField", "Recompute Model Field"),
    PLAINTEXT_TO_CIPHERTEXT("PlaintextToCiphertext", "Plaintext to Ciphertext");

    @JsonValue
    private final String code;
    private final String name;
}
