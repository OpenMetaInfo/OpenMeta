package info.openmeta.framework.base.exception;

import info.openmeta.framework.base.enums.ResponseCode;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Bad request exception, used for invalid input data.
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class IllegalArgumentException extends BaseException {

    private ResponseCode responseCode = ResponseCode.BAD_REQUEST;

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public IllegalArgumentException(String message, Object... args) {
        super(message, args);
    }
}
