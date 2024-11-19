package info.openmeta.framework.orm.datasource;

/**
 * Data source context holder.
 */
public class DataSourceContextHolder {

    private static final ThreadLocal<String> contextHolder = new ThreadLocal<>();

    /**
     * To set data source key.
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
     * Clear all the data source.
     */
    public static void clearDataSourceKey() {
        contextHolder.remove();
    }
}
