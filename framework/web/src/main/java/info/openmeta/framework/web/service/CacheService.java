package info.openmeta.framework.web.service;

import com.fasterxml.jackson.core.type.TypeReference;

import java.util.List;
import java.util.Map;

/**
 * Cache service
 * Support saving, searching, and deleting cache by key.
 */
public interface CacheService {

    /**
     * Save cache, use default expiration time.
     *
     * @param key cache key
     * @param object cache object
     */
    void save(String key, Object object);

    /**
     * Save cache, specify expiration time in seconds, 0 means permanent validity.
     *
     * @param key cache key
     * @param object cache object
     * @param expireSeconds expiration time in seconds
     */
    void save(String key, Object object, long expireSeconds);

    /**
     * Search cache by key list.
     *
     * @param keys key list
     * @return key-value map
     */
    Map<String, Object> search(List<String> keys);

    /**
     * Get the full key path.
     *
     * @param key cache key
     */
    String getKeyPath(String key);

    /**
     * Check if the key exists.
     *
     * @param key cache key
     * @return true or false
     */
    boolean hasKey(String key);

    /**
     * Get cache by key.
     *
     * @param key cache key
     * @return cache
     */
    String get(String key);

    /**
     * Get cache object by key, specify class for deserialization.
     *
     * @param key cache key
     * @param tClass class
     * @param <T> T
     * @return cache object
     */
    <T> T get(String key, Class<T> tClass);

    /**
     * Get cache object by key, specify TypeReference for deserialization.
     *
     * @param key cache key
     * @param typeReference TypeReference
     * @param <T> T
     * @return cache object
     */
    <T> T get(String key, TypeReference<T> typeReference);

    /**
     * Get cache object by key, specify TypeReference for deserialization, and return default value if not found.
     *
     * @param key cache key
     * @param typeReference TypeReference
     * @param defaultValue default value
     * @param <T> T
     * @return cache object
     */
    <T> T get(String key, TypeReference<T> typeReference, T defaultValue);

    /**
     * Increment count.
     * If the key does not exist, set the initial value and expiration time.
     *
     * @param key cache key
     * @param expiredSeconds expired seconds
     * @return count
     */
    Long increment(String key, long expiredSeconds);

    /**
     * Delete cache by key.
     *
     * @param key cache key
     * @return true if delete successfully, false otherwise
     */
    boolean delete(String key);

    /**
     * Delete key list.
     *
     * @param keys key list
     * @return delete count
     */
    Long delete(List<String> keys);

}
