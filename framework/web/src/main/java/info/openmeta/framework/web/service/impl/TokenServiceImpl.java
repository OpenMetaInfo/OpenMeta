package info.openmeta.framework.web.service.impl;

import info.openmeta.framework.web.service.CacheService;
import info.openmeta.framework.web.service.TokenService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.UUID;

/**
 * Token service implementation
 * Generate and validate token, support one-time token and temporary token.
 */
@Service
public class TokenServiceImpl implements TokenService {

    @Autowired
    private CacheService cacheService;

    /**
     * Generate token.
     *
     * @param key key
     * @param expireSeconds expiration time in seconds
     * @return token
     */
    @Override
    public String generateToken(String key, long expireSeconds) {
        String token = UUID.randomUUID().toString();
        cacheService.save(key + token, "1", expireSeconds);
        return token;
    }

    /**
     * Generate token for user or specific entity ID.
     *
     * @param key key
     * @param id entity ID
     * @param expireSeconds expiration time in seconds
     * @return token
     */
    @Override
    public String generateToken(String key, Long id, long expireSeconds) {
        String token = UUID.randomUUID().toString();
        cacheService.save(key + token, String.valueOf(id), expireSeconds);
        return token;
    }

    /**
     * Validate one-time token, and delete the token after successful validation.
     *
     * @param key key
     * @param token token
     * @return true or false
     */
    @Override
    public boolean validateOneTimeToken(String key, String token) {
        key = key + token;
        if (cacheService.hasKey(key)){
            cacheService.delete(key);
            return true;
        }
        return false;
    }

    /**
     * Validate one-time token, and delete the token after successful validation.
     * Entity ID is required for validation.
     *
     * @param key key
     * @param token token
     * @param id entity ID
     * @return true or false
     */
    @Override
    public boolean validateOneTimeToken(String key, String token, Long id) {
        key = key + token;
        String idStr = cacheService.get(key);
        if (StringUtils.isNotBlank(idStr) && Long.valueOf(idStr).equals(id)) {
            cacheService.delete(key);
            return true;
        }
        return false;
    }

    /**
     * Validate temporary token.
     *
     * @param key key
     * @param token token
     * @return true or false
     */
    @Override
    public boolean validateTempToken(String key, String token) {
        return cacheService.hasKey(key + token);
    }

    /**
     * Validate temporary token.
     * Entity ID is required for validation.
     *
     * @param key key
     * @param token token
     * @param id entity ID
     * @return true or false
     */
    @Override
    public boolean validateTempToken(String key, String token, Long id) {
        key = key + token;
        String idStr = cacheService.get(key + token);
        return StringUtils.isNotBlank(idStr) && Long.valueOf(idStr).equals(id);
    }

}
