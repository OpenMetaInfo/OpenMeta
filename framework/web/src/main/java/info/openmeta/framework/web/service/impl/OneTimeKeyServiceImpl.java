package info.openmeta.framework.web.service.impl;

import com.fasterxml.jackson.core.type.TypeReference;
import com.google.common.collect.Maps;
import info.openmeta.framework.base.constant.RedisConstant;
import info.openmeta.framework.orm.utils.RedisUtils;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.web.service.TempTokenService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

@Service
public class OneTimeKeyServiceImpl implements TempTokenService {

    private final static int TIME_TO_LIVE = 24;

    @Autowired
    private StringRedisTemplate redisTemplate;

    @Autowired
    private RedisUtils redisUtils;

    @Override
    public String saveTempToken(String token, int hours, Map<String, Object> variables) {
        String key = redisUtils.getRealKey(String.format("%s:%s", RedisConstant.ONE_TIME_KEY, token));
        redisTemplate.opsForValue().set(key, JsonMapper.objectToString(variables), TIME_TO_LIVE, TimeUnit.HOURS);
        return token;
    }

    @Override
    public String parseTempToken(String token) {
        String key = redisUtils.getRealKey(String.format("%s:%s", RedisConstant.ONE_TIME_KEY, token));
        return redisTemplate.opsForValue().get(key);
    }

    @Override
    public Map<String, String> parseTempToken(String type, String token) {
        String key = redisUtils.getRealKey(String.format("%s:%s", RedisConstant.ONE_TIME_KEY, token));
        String s = redisTemplate.opsForValue().get(key);
        if (StringUtils.isNotEmpty(s)) {
            return Maps.newHashMap();
        }
        return JsonMapper.stringToObject(s, new TypeReference<>() {});
    }

    @Override
    public String generateTempToken(Long id) {
        String token = UUID.randomUUID().toString();
        String key = redisUtils.getRealKey(String.format("%s:%s", RedisConstant.ONE_TIME_KEY, token));
        redisTemplate.opsForValue().set(key, String.valueOf(id), TIME_TO_LIVE, TimeUnit.HOURS);
        return token;
    }

    @Override
    public String generateTempToken(String type, Long id, int hours) {
        String token = UUID.randomUUID().toString();
        String key = redisUtils.getRealKey(String.format("%s:%s", RedisConstant.ONE_TIME_KEY, token));
        redisTemplate.opsForValue().set(key, String.valueOf(id), hours, TimeUnit.HOURS);
        return token;
    }

    @Override
    public String generateTempToken(Map<String, Long> objects, int hours) {
        String token = UUID.randomUUID().toString();
        String key = redisUtils.getRealKey(String.format("%s:%s", RedisConstant.ONE_TIME_KEY, token));
        redisTemplate.opsForValue().set(key, JsonMapper.objectToString(objects), hours, TimeUnit.HOURS);
        return token;
    }

    @Override
    public String generateTempToken(String type, Object obj, int minutes) {
        String token = UUID.randomUUID().toString();
        String key = redisUtils.getRealKey(String.format("%s:%s", RedisConstant.ONE_TIME_KEY, token));
        // Cache only if the expiration time is greater than 0.
        if (minutes > 0) {
            redisTemplate.opsForValue().set(key, JsonMapper.objectToString(obj), minutes, TimeUnit.MINUTES);
        }
        return token;
    }
}
