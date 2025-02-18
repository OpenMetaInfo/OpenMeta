package info.openmeta.framework.base.exception;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * System internal exception, mostly for unexpected errors, which might be caused by system bugs.
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class SystemException extends BaseException {

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public SystemException(String message, Object... args) {
        super(message, args);
    }
}
