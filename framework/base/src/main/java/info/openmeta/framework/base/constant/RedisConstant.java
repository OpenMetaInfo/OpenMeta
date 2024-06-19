package info.openmeta.framework.base.constant;

public interface RedisConstant {
    /** Expiration time */
    Long ONE_MINUTES = 60L;
    Long TEN_MINUTES = 10 * 60L;
    Long ONE_DAY = 24 * 60 * 60L;
    Long ONE_WEEK = 7 * 24 * 60 * 60L;

    /** redis key routes */
    String SESSION =  ":session:";
    String USER_INFO =  ":user-info:";
    String USER_PERMISSIONS =  ":user-permissions:";

    String ONE_TIME_KEY = ":one-time-key:";
}
