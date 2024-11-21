package info.openmeta.framework.orm.datasource;

/**
 * Data source context holder.
 */
public class DataSourceContextHolder {

    private static final ThreadLocal<String> contextHolder = new ThreadLocal<>();

    /**
     * To set datasource key for current thread.
     * @param key data source key
     */
    public static void setDataSourceKey(String key) {
        contextHolder.set(key);
    }

    /**
     * Get the key of current datasource.
     */
    public static String getDataSourceKey() {
        return contextHolder.get();
    }

    /**
     * Clear the datasource key, and route to the default datasource.
     */
    public static void clearDataSourceKey() {
        contextHolder.remove();
    }
}
