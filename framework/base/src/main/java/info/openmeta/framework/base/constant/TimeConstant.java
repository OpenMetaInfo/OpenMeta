package info.openmeta.framework.base.constant;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.Set;
import java.util.TimeZone;
import java.util.regex.Pattern;

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
    DateTimeFormatter SIMPLE_DATE_FORMATTER = DateTimeFormatter.ofPattern(SIMPLE_DATE_FORMAT);
    DateTimeFormatter TIME_FORMATTER = DateTimeFormatter.ofPattern(TIME_FORMAT);
    DateTimeFormatter SIMPLE_TIME_FORMATTER = DateTimeFormatter.ofPattern(SIMPLE_TIME_FORMAT);

    Set<Class<?>> DATE_TYPES = Set.of(LocalDate.class, LocalDateTime.class, Date.class);

    // regex for 2024
    Pattern PATTERN_YYYY = Pattern.compile("^((1[8-9]\\d{2})|([2-9]\\d{3}))$");
    // regex for 2024-09, 2024-9
    Pattern PATTERN_YYYY_MM = Pattern.compile("^((1[8-9]\\d{2})|([2-9]\\d{3}))-([1-9]|0[1-9]|1[0-2])$");
    // regex for 20240915
    Pattern PATTERN_SIMPLE_YYYYMMDD = Pattern.compile("^((1[8-9]\\d{2})|([2-9]\\d{3}))(0[1-9]|[12][0-9]|3[01])$");
    // regex for 2024-09-15, 2024-9-5
    Pattern PATTERN_YYYY_MM_DD = Pattern.compile(
            "^((1[8-9]\\d{2})|([2-9]\\d{3}))-([1-9]|0[1-9]|1[0-2])-([1-9]|0[1-9]|[12][0-9]|3[01])$");

    // regex for 2:30, 23:59
    Pattern PATTERN_HH_MM = Pattern.compile("^([0-9]|1[0-9]|2[0-3]):[0-5][0-9]$");
    // regex for 2:30:15, 23:59:59
    Pattern PATTERN_HH_MM_SS = Pattern.compile("^([0-9]|1[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$");

}
