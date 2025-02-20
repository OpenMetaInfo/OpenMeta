package info.openmeta.framework.base.exception;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Security exception
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class SecurityException extends BaseException {

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public SecurityException(String message, Object... args) {
        super(message, args);
    }
}
