package info.openmeta.framework.base.exception;

import info.openmeta.framework.base.enums.ResponseCode;
import info.openmeta.framework.base.i18n.I18n;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.slf4j.event.Level;

@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class BaseException extends RuntimeException {

    private String message;
    private Level logLevel = Level.ERROR;
    private ResponseCode responseCode = ResponseCode.ERROR;

    /**
     * Accept variable parameter exception message.
     * @param message: Exception message, accept placeholder message, support i18n
     * @param args:    Variable parameters, support the last parameter as Throwable object
     */
    public BaseException(String message, Object... args) {
        super(message);
        message = I18n.get(message, args);
        if (args.length > 0) {
            // Determine whether the last parameter is a Throwable object,
            // if so, use it as the cause and output it to the log with the current exception object
            Object lastArg = args[args.length - 1];
            if (lastArg instanceof Throwable) {
                this.initCause((Throwable) lastArg);
            }
        }
        this.message = message;
    }
}
