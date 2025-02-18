package info.openmeta.framework.base.exception;

import info.openmeta.framework.base.enums.ResponseCode;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Validation exception, used for invalid input data.
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class ValidationException extends BaseException {

    private ResponseCode responseCode = ResponseCode.BAD_REQUEST;

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public ValidationException(String message, Object... args) {
        super(message, args);
    }
}
