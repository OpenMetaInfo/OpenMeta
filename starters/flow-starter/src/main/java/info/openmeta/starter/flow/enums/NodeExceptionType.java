package info.openmeta.starter.flow.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Flow Node Exception Type
 * Check the exception condition and throw an exception.
 */
@Getter
@AllArgsConstructor
public enum NodeExceptionType {
    RESULT_IS_EMPTY("ResultIsEmpty", "Result Is Empty"),
    RESULT_IS_EMPTY_OR_ZERO("ResultIsEmptyOrZero", "Result Is Empty Or Zero"),
    RESULT_IS_NOT_EMPTY("ResultIsNotEmpty", "Result Is Not Empty"),
    RESULT_IS_FALSE("ResultIsFalse", "Result Is False"),
    RESULT_IS_TRUE("ResultIsTrue", "Result Is True");

    @JsonValue
    private final String type;
    private final String name;
}
