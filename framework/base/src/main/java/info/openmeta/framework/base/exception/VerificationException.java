package info.openmeta.framework.base.exception;

import info.openmeta.framework.base.enums.ResponseCode;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.slf4j.event.Level;

/**
 * Verification exception, such as sms code, email code, etc.
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class VerificationException extends BaseException {

    private Level logLevel = Level.WARN;
    private ResponseCode responseCode = ResponseCode.VERIFICATION_EXCEPTION;

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public VerificationException(String message, Object... args) {
        super(message, args);
    }
}
