package info.openmeta.framework.base.exception;

import info.openmeta.framework.base.enums.ResponseCode;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.slf4j.event.Level;

/**
 * Business exception
 * The message would be shown to the user.
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class BusinessException extends BaseException {

    private Level logLevel = Level.WARN;
    private ResponseCode responseCode = ResponseCode.BUSINESS_EXCEPTION;

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public BusinessException(String message, Object... args) {
        super(message, args);
    }

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public BusinessException(ResponseCode responseCode, String message, Object... args) {
        super(message, args);
        this.responseCode = responseCode;
    }
}
