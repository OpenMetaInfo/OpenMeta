package info.openmeta.framework.orm.datasource;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Readonly datasource holder.
 * Used to randomly get a readonly datasource key.
 */
public class ReadonlyDataSourceHolder {

    private static final List<String> READONLY_DATA_SOURCE_KEYS = new ArrayList<>();

    /**
     * Add a readonly datasource key.
     *
     * @param key datasource key
     */
    public static void addReadonlyDataSourceKey(String key) {
        READONLY_DATA_SOURCE_KEYS.add(key);
    }

    /**
     * Get a readonly datasource key randomly.
     *
     * @return readonly datasource key
     */
    public static synchronized String getReadonlyDataSourceKey() {
        if (READONLY_DATA_SOURCE_KEYS.isEmpty()) {
            throw new RuntimeException("No readonly datasource available.");
        }
        int index = ThreadLocalRandom.current().nextInt(READONLY_DATA_SOURCE_KEYS.size());
        return READONLY_DATA_SOURCE_KEYS.get(index);
    }
}
