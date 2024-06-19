package info.openmeta.framework.base.exception;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * Configuration exception
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class ConfigurationException extends BaseException {

    /**
     * Accepts variable arguments, optionally ending with a Throwable for enhanced error tracking.
     */
    public ConfigurationException(String message, Object... args){
        super(message, args);
    }
}
