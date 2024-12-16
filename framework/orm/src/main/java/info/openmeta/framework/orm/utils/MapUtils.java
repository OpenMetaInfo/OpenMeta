package info.openmeta.framework.orm.utils;

import com.google.common.collect.ImmutableMap;
import info.openmeta.framework.base.utils.SFunction;
import info.openmeta.framework.base.utils.Cast;

import java.util.HashMap;
import java.util.Map;

/**
 * Map utility class
 */
public class MapUtils {

    private MapUtils() {}

    /**
     * Build a Map with only one key-value pair.
     *
     * @param key the key
     * @param value the value
     * @return the map
     */
    public static Map<String, Object> of(String key, Object value) {
        Map<String, Object> map = new HashMap<>();
        map.put(key, value);
        return map;
    }

    /**
     * Build a Map with only one key-value pair.
     *
     * @param method the `get method` of the field, Lambda expression, method reference
     * @param value the value
     * @return the map
     */
    public static <T, R> Map<String, Object> of(SFunction<T, R> method, Object value) {
        String field = LambdaUtils.getAttributeName(method);
        return of(field, value);
    }

    /**
     * Build a Map with only one key-value pair.
     *
     * @return Map instance
     */
    public static ImmutableMap<String, Object> ofImmutable(String key, Object value) {
        Map<String, Object> map = of(key, value);
        return ImmutableMap.copyOf(map);
    }

    /**
     * Quick build Map<String, Object> type static factory method, for example,
     *      MapUtils.strObj().put(k1, v1).put(k2, v2).build()
     *
     * @return MapBuilder instance
     */
    public static MapBuilder<String, Object> strObj() {
        return new MapBuilder<>();
    }

    /**
     * Quick build Map<String, String> type static factory method, for example,
     *      MapUtils.strStr().put(k1, v1).put(k2, v2).build()
     *
     * @return MapBuilder instance
     */
    public static MapBuilder<String, String> strStr() {
        return new MapBuilder<>();
    }

    /**
     * Custom key-value data type static factory method for MapBuilder, for example,
     *      MapUtils.<Long, String>builder().put(k1, v1).put(k2, v2).build()
     *
     * @return an instance of MapBuilder class
     * @param <K> key type
     * @param <V> value type
     */
    public static <K, V> MapBuilder<K, V> builder() {
        return new MapBuilder<>();
    }

    /**
     * An inner static class used for quickly building Maps and ImmutableMap.
     * @param <K> the type of keys
     * @param <V> the type of values
     */
    public static class MapBuilder<K, V> {
        private final Map<K, V> map = new HashMap<>();

        public MapBuilder<K, V> put(K key, V value) {
            map.put(key, value);
            return this;
        }

        public <T, R> MapBuilder<K, V> put(SFunction<T, R> method, V value) {
            String field = LambdaUtils.getAttributeName(method);
            map.put(Cast.of(field), value);
            return this;
        }

        /**
         * Build HashMap
         * @return Map
         */
        public Map<K, V> build() {
            return map;
        }

        /**
         * Build ImmutableMap
         * @return ImmutableMap
         */
        public ImmutableMap<K, V> buildImmutableMap() {
            return ImmutableMap.copyOf(map);
        }
    }
}
