package info.openmeta.framework.base.exception;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Flow exception
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class FlowException extends BaseException {

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public FlowException(String message, Object... args){
        super(message, args);
    }
}
