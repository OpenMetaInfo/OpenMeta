package info.openmeta.framework.base.exception;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Integration exception
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class IntegrationException extends BaseException {

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public IntegrationException(String message, Object... args) {
        super(message, args);
    }
}
