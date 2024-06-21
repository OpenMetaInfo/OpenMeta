package info.openmeta.framework.orm.utils;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

/**
 * Redis utility class
 */
@Component
@Slf4j
public class RedisUtils {

    @Value("${spring.data.redis.root-key:metadata-framework}")
    private String rootKey;

    @Autowired
    private RedisTemplate<Object, Object> redisTemplate;

    public String getRealKey(String key) {
        return rootKey + key;
    }

    /**
     * Set the expiration time of the specified cache key
     *
     * @param key  key
     * @param time time (seconds)
     * @return true success, false failure
     */
    public boolean expire(String key, long time) {
        try {
            if (time >= 0) {
                redisTemplate.expire(key, time, TimeUnit.SECONDS);
            }
            return true;
        } catch (Exception e) {
            log.error("RedisUtil expire method error! key: " + key, e);
            return false;
        }
    }

    /**
     * Get the expiration time of the specified cache key
     *
     * @param key key, cannot be null
     * @return time (seconds) 0 means permanent validity
     */
    public long getExpire(String key) {
        return Optional.ofNullable(redisTemplate.getExpire(key, TimeUnit.SECONDS)).orElse(0L);
    }

    /**
     * Determine whether the key exists
     *
     * @param key key
     * @return true or false
     */
    public boolean hasKey(String key) {
        try {
            return Optional.ofNullable(redisTemplate.hasKey(key)).orElse(false);
        } catch (Exception e) {
            log.error("RedisUtil hasKey method error! key: " + key, e);
            return false;
        }
    }

    /**
     * Delete cache
     *
     * @param keys can pass one or more values
     * @return true success, false failure
     */
    public boolean del(List<String> keys) {
        if (!keys.isEmpty()) {
            return Optional.ofNullable(redisTemplate.delete(keys)).orElse(false);
        }
        return false;
    }

    /**
     * Get the value of the specified cache key
     *
     * @param key key
     * @return value
     */
    public Object get(String key) {
        return key == null ? null : redisTemplate.opsForValue().get(key);
    }

    /**
     * Put the specified cache key
     *
     * @param key   key
     * @param value value
     * @return true success, false failure
     */
    public boolean set(String key, Object value) {
        try {
            redisTemplate.opsForValue().set(key, value);
            return true;
        } catch (Exception e) {
            log.error("RedisUtil set method error! key: " + key, e);
            return false;
        }
    }

    /**
     * Put the specified cache key and set the expiration time
     *
     * @param key   key
     * @param value value
     * @param time  time (seconds) time must be greater than 0, if time is less than or equal to 0, it will be set indefinitely
     * @return true success, false failure
     */
    public boolean set(String key, Object value, long time) {
        try {
            if (time > 0) {
                redisTemplate.opsForValue().set(key, value, time, TimeUnit.SECONDS);
            } else {
                set(key, value);
            }
            return true;
        } catch (Exception e) {
            log.error("RedisUtil set method error! key: " + key, e);
            return false;
        }
    }
}


