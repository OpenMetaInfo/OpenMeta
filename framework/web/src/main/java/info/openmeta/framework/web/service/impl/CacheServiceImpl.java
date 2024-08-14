package info.openmeta.framework.web.service.impl;

import com.fasterxml.jackson.core.type.TypeReference;
import info.openmeta.framework.base.constant.RedisConstant;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.web.service.CacheService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * Cache service implementation.
 * Support saving, searching, and deleting cache by key.
 */
@Service
@Slf4j
public class CacheServiceImpl implements CacheService {

    /**
     * Root key, such as: "openmeta:"
     */
    @Value("${spring.data.redis.root-key:}")
    private String rootKey;

    @Autowired
    private StringRedisTemplate stringRedisTemplate;

    /**
     * Save cache, use default expiration time.
     *
     * @param key cache key
     * @param object cache object
     */
    @Override
    public void save(String key, Object object) {
        this.save(key, object, RedisConstant.DEFAULT_EXPIRE_SECONDS);
    }

    /**
     * Save cache, specify expiration time in seconds, 0 means permanent validity.
     *
     * @param key cache key
     * @param object cache object
     * @param expireSeconds expiration time in seconds
     */
    @Override
    public void save(String key, Object object, long expireSeconds) {
        String cacheKey = this.getKeyPath(key);
        String value = JsonMapper.objectToString(object);
        if (expireSeconds < 0) {
            log.warn("Invalid expiration time, use default expiration seconds: {}", RedisConstant.DEFAULT_EXPIRE_SECONDS);
            expireSeconds = RedisConstant.DEFAULT_EXPIRE_SECONDS;
        }
        stringRedisTemplate.opsForValue().set(cacheKey, value, expireSeconds, TimeUnit.SECONDS);
    }

    /**
     * Search cache by key list.
     *
     * @param keys key list
     * @return key-value map
     */
    @Override
    public Map<String, Object> search(List<String> keys) {
        Map<String, Object> map = new HashMap<>();
        for (String key : keys) {
            map.put(key, stringRedisTemplate.opsForValue().get(key));
        }
        return map;
    }

    /**
     * Get the full key path.
     *
     * @param key cache key
     */
    @Override
    public String getKeyPath(String key) {
        return rootKey + key;
    }

    /**
     * Check if the key exists.
     *
     * @param key cache key
     * @return true or false
     */
    @Override
    public boolean hasKey(String key) {
        return Boolean.TRUE.equals(stringRedisTemplate.hasKey(key));
    }

    /**
     * Get cache by key.
     *
     * @param key cache key
     * @return cache
     */
    @Override
    public String get(String key) {
        String cacheKey = this.getKeyPath(key);
        return stringRedisTemplate.opsForValue().get(cacheKey);
    }

    /**
     * Get cache object by key, specify class for deserialization.
     *
     * @param key cache key
     * @param tClass class
     * @param <T> T
     * @return cache object
     */
    @Override
    public <T> T get(String key, Class<T> tClass) {
        String cacheKey = this.getKeyPath(key);
        String value = stringRedisTemplate.opsForValue().get(cacheKey);
        if (StringUtils.hasText(value)) {
            return JsonMapper.stringToObject(value, tClass);
        }
        return null;
    }

    /**
     * Get cache object by key, specify TypeReference for deserialization.
     *
     * @param key cache key
     * @param typeReference TypeReference
     * @param <T> T
     * @return cache object
     */
    @Override
    public <T> T get(String key, TypeReference<T> typeReference) {
        String cacheKey = this.getKeyPath(key);
        String value = stringRedisTemplate.opsForValue().get(cacheKey);
        if (StringUtils.hasText(value)) {
            return JsonMapper.stringToObject(value, typeReference);
        }
        return null;
    }

    /**
     * Get cache object by key, specify TypeReference for deserialization, and return default value if not found.
     *
     * @param key cache key
     * @param typeReference TypeReference
     * @param defaultValue default value
     * @param <T> T
     * @return cache object
     */
    @Override
    public <T> T get(String key, TypeReference<T> typeReference, T defaultValue) {
        String cacheKey = this.getKeyPath(key);
        String value = stringRedisTemplate.opsForValue().get(cacheKey);
        if (StringUtils.hasText(value)) {
            return JsonMapper.stringToObject(value, typeReference);
        }
        return defaultValue;
    }

    /**
     * Increment count.
     * If the key does not exist, set the initial value and expiration time.
     *
     * @param key cache key
     * @param expiredSeconds expired seconds
     * @return count
     */
    @Override
    public Long increment(String key, long expiredSeconds) {
        key = getKeyPath(key);
        ValueOperations<String, String> valueOperations = stringRedisTemplate.opsForValue();
        if (Boolean.TRUE.equals(stringRedisTemplate.hasKey(key))) {
            return valueOperations.increment(key);
        } else {
            // Set the initial value and expiration time in seconds.
            valueOperations.set(key, "1", expiredSeconds, TimeUnit.SECONDS);
            return 1L;
        }
    }

    /**
     * Delete cache by key.
     *
     * @param key cache key
     * @return boolean
     */
    @Override
    public boolean delete(String key) {
        key = getKeyPath(key);
        return Boolean.TRUE.equals(stringRedisTemplate.delete(key));
    }

    /**
     * Delete key list.
     *
     * @param keys key list
     * @return delete count
     */
    @Override
    public Long delete(List<String> keys) {
        keys = keys.stream().map(this::getKeyPath).collect(Collectors.toList());
        return stringRedisTemplate.delete(keys);
    }

}
