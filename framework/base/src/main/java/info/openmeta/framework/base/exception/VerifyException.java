package info.openmeta.framework.base.exception;

import info.openmeta.framework.base.enums.ResponseCode;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.slf4j.event.Level;

/**
 * Verify exception, such as sms code, email code, etc.
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class VerifyException extends BaseException {

    private Level logLevel = Level.WARN;
    private ResponseCode responseCode = ResponseCode.VERIFY_EXCEPTION;

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public VerifyException(String message, Object... args){
        super(message, args);
    }
}
