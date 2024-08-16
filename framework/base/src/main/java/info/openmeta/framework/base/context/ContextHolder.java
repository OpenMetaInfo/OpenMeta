package info.openmeta.framework.base.context;

import info.openmeta.framework.base.utils.JsonMapper;
import org.springframework.core.NamedThreadLocal;

public final class ContextHolder {

    private static final ThreadLocal<Context> LOCAL_THREAD_CONTEXT = new NamedThreadLocal<>("Local Thread Context");

    private ContextHolder() {}

    /**
     * Convert json text to Context object, and add it to threadLocal.
     * @param jsonContext json string format of Context
     */
    public static void setContext(String jsonContext) {
        Context context = JsonMapper.stringToObject(jsonContext, Context.class);
        LOCAL_THREAD_CONTEXT.set(context);
    }

    public static void setContext(Context context) {
        LOCAL_THREAD_CONTEXT.set(context);
    }

    /**
     * Get the context of the current thread, if it is null, return a new context.
     *
     * @return context
     */
    public static Context getContext() {
        Context context = LOCAL_THREAD_CONTEXT.get();
        return context == null ? new Context() : context;
    }

    /**
     * Check if the current thread has a context.
     *
     * @return true if the current thread has a context
     */
    public static boolean existContext() {
        return LOCAL_THREAD_CONTEXT.get() != null;
    }

    /**
     * Clone the current context, if it is null, return a new context.
     *
     * @return cloned context
     */
    public static Context cloneContext() {
        Context context = LOCAL_THREAD_CONTEXT.get();
        return context == null ? new Context() : context.copy();
    }

    /**
     * Remove value in threadLocal at the end of the current request.
     */
    public static void removeContext() {
        LOCAL_THREAD_CONTEXT.remove();
    }
}
