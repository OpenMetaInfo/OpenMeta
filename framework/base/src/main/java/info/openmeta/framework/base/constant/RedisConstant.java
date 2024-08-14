package info.openmeta.framework.base.constant;

public interface RedisConstant {
    /** Expiration time */
    long ONE_MINUTES = 60L;
    long TEN_MINUTES = 10 * 60L;
    long ONE_DAY = 24 * 60 * 60L;
    long ONE_WEEK = 7 * 24 * 60 * 60L;

    // The default expiration time of the cache is ONE_DAY.
    long DEFAULT_EXPIRE_SECONDS = ONE_DAY;


    /** redis key routes */
    String SESSION =  "session:";
    String USER_INFO =  "user-info:";
    String USER_PERMISSIONS =  "user-permissions:";


    String TEMP_TOKEN = "temp-token:";
    String ONE_TIME_KEY = "one-time-key:";
}
