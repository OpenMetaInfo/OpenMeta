package info.openmeta.framework.base.exception;

import info.openmeta.framework.base.enums.ResponseCode;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * External system exception
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class ExternalException extends BaseException {

    private ResponseCode responseCode = ResponseCode.BUSINESS_EXCEPTION;

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public ExternalException(String message, Object... args){
        super(message, args);
    }

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public ExternalException(ResponseCode responseCode, String message, Object... args) {
        super(message, args);
        this.responseCode = responseCode;
    }
}
