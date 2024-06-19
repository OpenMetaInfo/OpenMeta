package info.openmeta.framework.base.exception;

import info.openmeta.framework.base.enums.ResponseCode;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Data consistency exception, such as version conflict.
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class VersionException extends BaseException {

    private ResponseCode responseCode = ResponseCode.VERSION_CHANGED;

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public VersionException(String message, Object... args){
        super(message, args);
    }
}
