package info.openmeta.framework.web.service.impl;

import com.fasterxml.jackson.core.type.TypeReference;
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

@Service
@Slf4j
public class CacheServiceImpl implements CacheService {

    @Value("${spring.data.redis.root-key:}")
    private String rootKey;

    @Autowired
    private StringRedisTemplate stringRedisTemplate;

    @Override
    public void save(String key, Object object) {
        String cacheKey = this.getKeyPath(key);
        stringRedisTemplate.opsForValue().set(cacheKey, JsonMapper.objectToString(object), 24, TimeUnit.HOURS);
    }

    @Override
    public void save(String key, Object object, Long expireSeconds) {
        String cacheKey = this.getKeyPath(key);
        stringRedisTemplate.opsForValue().set(cacheKey, JsonMapper.objectToString(object), expireSeconds, TimeUnit.SECONDS);
    }

    @Override
    public Map<String, Object> search(List<String> keys) {
        Map<String, Object> map = new HashMap<>();
        for (String key : keys) {
            map.put(key, stringRedisTemplate.opsForValue().get(key));
        }
        return map;
    }

    /**
     * Get the full key path
     * @param key cache key
     */
    @Override
    public String getKeyPath(String key) {
        return rootKey + key;
    }

    @Override
    public <T> T get(String key, Class<T> tClass) {
        String cacheKey = this.getKeyPath(key);
        String value = stringRedisTemplate.opsForValue().get(cacheKey);
        if (StringUtils.hasText(value)) {
            return JsonMapper.stringToObject(value, tClass);
        }
        return null;
    }

    @Override
    public <T> T get(String key, TypeReference<T> typeReference) {
        String cacheKey = this.getKeyPath(key);
        String value = stringRedisTemplate.opsForValue().get(cacheKey);
        if (StringUtils.hasText(value)) {
            return JsonMapper.stringToObject(value, typeReference);
        }
        return null;
    }

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
     * Increment count
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
     * Delete cache by key
     * @param key cache key
     * @return boolean
     */
    @Override
    public Boolean delete(String key) {
        key = getKeyPath(key);
        return stringRedisTemplate.delete(key);
    }

    /**
     * Delete key list
     * @param keys key list
     * @return delete count
     */
    @Override
    public Long delete(List<String> keys) {
        keys = keys.stream().map(this::getKeyPath).collect(Collectors.toList());
        return stringRedisTemplate.delete(keys);
    }

}
