package info.openmeta.framework.web.service;

/**
 * Token service
 * Support generating token, validating one-time token, and validating temporary token.
 */
public interface TokenService {

    /**
     * Generate token
     *
     * @param key key
     * @param expireSeconds expiration time in seconds
     * @return token
     */
    String generateToken(String key, long expireSeconds);

    /**
     * Generate token for user or specific entity ID.
     *
     * @param key key
     * @param id entity ID
     * @param expireSeconds expiration time in seconds
     * @return token
     */
    String generateToken(String key, Long id, long expireSeconds);

    /**
     * Validate one-time token, and delete the token after successful validation.
     *
     * @param key key
     * @param token token
     * @return true or false
     */
    boolean validateOneTimeToken(String key, String token);

    /**
     * Validate one-time token, and delete the token after successful validation.
     * Entity ID is required for validation.
     *
     * @param key key
     * @param token token
     * @param id entity ID
     * @return true or false
     */
    boolean validateOneTimeToken(String key, String token, Long id);

    /**
     * Validate temporary token
     *
     * @param key key
     * @param token token
     * @return true or false
     */
    boolean validateTempToken(String key, String token);

    /**
     * Validate temporary token.
     * Entity ID is required for validation.
     *
     * @param key key
     * @param token token
     * @param id entity ID
     * @return true or false
     */
    boolean validateTempToken(String key, String token, Long id);
}
