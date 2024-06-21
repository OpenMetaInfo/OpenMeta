package info.openmeta.framework.orm.changelog;

import info.openmeta.framework.orm.changelog.message.dto.ChangeLog;
import org.springframework.core.NamedThreadLocal;

import java.util.List;

/**
 * ThreadLocal variable of ChangeLog, used to temporarily store the ChangeLog data
 */
public class ChangeLogHolder {

    private ChangeLogHolder() {}

    private static final ThreadLocal<List<ChangeLog>> CHANGE_LOG_THREAD_LOCAL = new NamedThreadLocal<>("ChangeLog ThreadLocal");

    /**
     * Check whether the threadLocal variable is empty
     */
    public static boolean isEmpty() {
        return CHANGE_LOG_THREAD_LOCAL.get() == null || CHANGE_LOG_THREAD_LOCAL.get().isEmpty();
    }

    /**
     * Get the ChangeLog list from the threadLocal variable
     */
    public static List<ChangeLog> get() {
        return CHANGE_LOG_THREAD_LOCAL.get();
    }

    /**
     * Set the ChangeLog list to the threadLocal variable
     * @param changeLogs ChangeLog list
     */
    public static void set(List<ChangeLog> changeLogs) {
        CHANGE_LOG_THREAD_LOCAL.set(changeLogs);
    }

    /**
     * Add ChangeLog to the threadLocal variable
     * @param changeLogs ChangeLog list
     */
    public static void add(List<ChangeLog> changeLogs) {
        CHANGE_LOG_THREAD_LOCAL.get().addAll(changeLogs);
    }

    /**
     * Clear the threadLocal variable
     */
    public static void clear() {
        CHANGE_LOG_THREAD_LOCAL.remove();
    }
}
