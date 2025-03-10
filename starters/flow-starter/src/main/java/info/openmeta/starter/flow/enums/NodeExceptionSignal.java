package info.openmeta.starter.flow.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Flow Node Exception Signal
 * Optional to terminate the current Node or the entire Flow.
 * When terminating the flow or one node, subsequent nodes will not be executed,
 * and the transaction will not be rolled back.
 */
@Getter
@AllArgsConstructor
public enum NodeExceptionSignal {
    SKIP_NODE("SkipNode", "Skip Current Node"),
    END_FLOW("EndFlow", "End Current Flow"),
    END_LOOP_NODE("EndLoopNode", "End Current Loop Node"),
    THROW_EXCEPTION("ThrowException", "Throw Exception"),
    LOG_ERROR("LogError", "Log Error");

    @JsonValue
    private final String code;
    private final String name;
}
