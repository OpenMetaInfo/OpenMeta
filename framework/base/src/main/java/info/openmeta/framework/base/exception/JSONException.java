package info.openmeta.framework.base.exception;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * JSON conversion exception
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class JSONException extends BaseException {

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public JSONException(String message, Object... args) {
        super(message, args);
    }
}
