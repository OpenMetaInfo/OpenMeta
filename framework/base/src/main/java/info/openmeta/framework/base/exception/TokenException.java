package info.openmeta.framework.base.exception;

import info.openmeta.framework.base.enums.ResponseCode;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.slf4j.event.Level;

/**
 * Token exception, used for token expired or invalid
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class TokenException extends BaseException {

    private Level logLevel = Level.ERROR;
    private ResponseCode responseCode = ResponseCode.TOKEN_EXPIRED;

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public TokenException(String message, Object... args){
        super(message, args);
    }
}
