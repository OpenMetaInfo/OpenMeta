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
        if (args == null || args.length == 0) {
            message = I18n.get(message);
        } else if (args[args.length - 1] instanceof Throwable) {
            // Determine whether the last parameter is a Throwable object,
            // If so, use it as the cause and output it to the log with the current exception object
            this.initCause((Throwable) args[args.length - 1]);
            // Remove the last parameter to avoid translation errors
            if (args.length == 1) {
                message = I18n.get(message);
            } else {
                Object[] newArgs = new Object[args.length - 1];
                System.arraycopy(args, 0, newArgs, 0, args.length - 1);
                message = I18n.get(message, newArgs);
            }
        } else {
            message = I18n.get(message, args);
        }
        this.message = message;
    }
}
