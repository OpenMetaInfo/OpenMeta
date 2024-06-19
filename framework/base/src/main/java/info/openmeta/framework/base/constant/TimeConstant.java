package info.openmeta.framework.base.constant;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.Set;
import java.util.TimeZone;

public interface TimeConstant {
    String NOW = "NOW";

    String DEFAULT_TIMEZONE_STRING = "UTC";
    ZoneId DEFAULT_TIMEZONE_ID = ZoneId.of(DEFAULT_TIMEZONE_STRING);
    TimeZone DEFAULT_TIMEZONE = TimeZone.getTimeZone(DEFAULT_TIMEZONE_ID);
    LocalDateTime EPOCH_TIME = LocalDateTime.of(1970, 1, 1, 0, 0, 0);

    String DATETIME_FORMAT = "yyyy-MM-dd HH:mm:ss";
    String DATE_FORMAT = "yyyy-MM-dd";
    String TIME_FORMAT = "HH:mm:ss";
    String SIMPLE_DATE_FORMAT = "yyyyMMdd";
    String SIMPLE_TIME_FORMAT = "HH:mm";

    DateTimeFormatter DATETIME_FORMATTER = DateTimeFormatter.ofPattern(DATETIME_FORMAT);
    DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern(DATE_FORMAT);
    DateTimeFormatter TIME_FORMATTER = DateTimeFormatter.ofPattern(TIME_FORMAT);
    DateTimeFormatter SIMPLE_TIME_FORMATTER = DateTimeFormatter.ofPattern(SIMPLE_TIME_FORMAT);

    Set<Class<?>> DATE_TYPES = Set.of(LocalDate.class, LocalDateTime.class, Date.class);
}
