package info.openmeta.framework.web.service;

import com.fasterxml.jackson.core.type.TypeReference;

import java.util.List;
import java.util.Map;

public interface CacheService {

    void save(String key, Object object);

    void save(String key, Object object, Long expireSeconds);

    Map<String, Object> search(List<String> keys);

    /**
     * Get the full key path
     * @param key cache key
     */
    String getKeyPath(String key);

    <T> T get(String key, Class<T> tClass);

    <T> T get(String key, TypeReference<T> typeReference);

    <T> T get(String key, TypeReference<T> typeReference, T defaultValue);

    /**
     * Increment count
     * @param key cache key
     * @param expiredSeconds expired seconds
     * @return count
     */
    Long increment(String key, long expiredSeconds);

    /**
     * Delete cache by key
     * @param key cache key
     * @return boolean
     */
    Boolean delete(String key);

    /**
     * Delete key list
     * @param keys key list
     * @return delete count
     */
    Long delete(List<String> keys);

}
