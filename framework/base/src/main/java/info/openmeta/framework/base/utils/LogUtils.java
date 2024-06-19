package info.openmeta.framework.base.utils;

/**
 * Log utility class
 */
public class LogUtils {

    /** Maximum length of a log message */
    private static final Integer MAX_LOG_LENGTH = 1000;

    /**
     * Split the log content length, to prevent the log content from being too long, such as large JSON, file content, etc.
     */
    public static String splitLog(String content) {
        if (content != null && content.length() > MAX_LOG_LENGTH) {
            content = content.substring(0, MAX_LOG_LENGTH) + " ...";
        }
        return content;
    }
}
