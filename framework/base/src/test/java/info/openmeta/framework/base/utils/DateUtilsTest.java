package info.openmeta.framework.base.utils;

import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.*;

class DateUtilsTest {

    @Test
    void getInstantNowReturnsCurrentInstant() {
        Instant now = DateUtils.getInstantNow();
        assertNotNull(now);
    }

    @Test
    void getZonedDateTimeNowReturnsCurrentZonedDateTime() {
        ZonedDateTime now = DateUtils.getZonedDateTimeNow("UTC");
        assertNotNull(now);
    }

    @Test
    void getZonedDateTimeNowStringReturnsCurrentZonedDateTimeString() {
        String nowString = DateUtils.getZonedDateTimeNowString("UTC");
        assertNotNull(nowString);
    }

    @Test
    void getCurrentLocalDateStringReturnsCurrentDateString() {
        String dateString = DateUtils.getCurrentLocalDateString("UTC");
        assertNotNull(dateString);
    }

    @Test
    void getCurrentSimpleDateStringReturnsCurrentDateString() {
        String dateString = DateUtils.getCurrentSimpleDateString();
        assertNotNull(dateString);
    }

    @Test
    void getCurrentLocalTimeStringReturnsCurrentTimeString() {
        String timeString = DateUtils.getCurrentLocalTimeString("UTC");
        assertNotNull(timeString);
    }

    @Test
    void getCurrentYearReturnsCurrentYear() {
        Integer year = DateUtils.getCurrentYear("UTC");
        assertNotNull(year);
    }

    @Test
    void getCurrentMonthReturnsCurrentMonth() {
        Integer month = DateUtils.getCurrentMonth("UTC");
        assertNotNull(month);
    }

    @Test
    void getCurrentDayReturnsCurrentDay() {
        Integer day = DateUtils.getCurrentDay("UTC");
        assertNotNull(day);
    }

    @Test
    void instantToLocalDateConvertsInstantToLocalDate() {
        Instant now = Instant.now();
        LocalDate localDate = DateUtils.instantToLocalDate(now);
        assertNotNull(localDate);
    }

    @Test
    void instantToLocalDateTimeConvertsInstantToLocalDateTime() {
        Instant now = Instant.now();
        LocalDateTime localDateTime = DateUtils.instantToLocalDateTime(now);
        assertNotNull(localDateTime);
    }

    @Test
    void stringToDateObjectConvertsStringToLocalDate() {
        Object date = DateUtils.stringToDateObject("2023-09-15", LocalDate.class);
        assertInstanceOf(LocalDate.class, date);
    }

    @Test
    void stringToDateObjectConvertsStringToLocalDateTime() {
        Object dateTime = DateUtils.stringToDateObject("2023-09-15 12:30:00", LocalDateTime.class);
        assertInstanceOf(LocalDateTime.class, dateTime);
    }

    @Test
    void stringToDateObjectConvertsStringToDate() {
        Object date = DateUtils.stringToDateObject("2023-09-15 12:30:00", Date.class);
        assertInstanceOf(Date.class, date);
    }

    @Test
    void stringToLocalDateConvertsStringToLocalDate() {
        LocalDate localDate = DateUtils.stringToLocalDate("2023-09-15");
        assertNotNull(localDate);
    }

    @Test
    void stringToLocalDateTimeConvertsStringToLocalDateTime() {
        LocalDateTime localDateTime = DateUtils.stringToLocalDateTime("2023-09-15 12:30:00");
        assertNotNull(localDateTime);
    }

    @Test
    void dateToStringConvertsDateToString() {
        String dateString = DateUtils.dateToString(LocalDate.of(2023, 9, 15));
        assertNotNull(dateString);
    }

    @Test
    void dateTimeToStringConvertsDateTimeToString() {
        String dateTimeString = DateUtils.dateTimeToString(LocalDateTime.of(2023, 9, 15, 12, 30));
        assertNotNull(dateTimeString);
    }

    @Test
    void dateToLocalDateConvertsDateToLocalDate() {
        LocalDate localDate = DateUtils.dateToLocalDate(new Date());
        assertNotNull(localDate);
    }

    @Test
    void dateToLocalDateTimeConvertsDateToLocalDateTime() {
        LocalDateTime localDateTime = DateUtils.dateToLocalDateTime(new Date());
        assertNotNull(localDateTime);
    }

    @Test
    void betweenReturnsTrueForDateWithinRange() {
        LocalDate date = LocalDate.of(2023, 9, 15);
        LocalDate startDate = LocalDate.of(2023, 9, 1);
        LocalDate endDate = LocalDate.of(2023, 9, 30);
        assertTrue(DateUtils.between(date, startDate, endDate));
    }

    @Test
    void betweenReturnsFalseForDateOutsideRange() {
        LocalDate date = LocalDate.of(2023, 10, 1);
        LocalDate startDate = LocalDate.of(2023, 9, 1);
        LocalDate endDate = LocalDate.of(2023, 9, 30);
        assertFalse(DateUtils.between(date, startDate, endDate));
    }

    @Test
    void padZeroPadsSingleDigitValue() {
        String paddedValue = DateUtils.padZero(5);
        assertEquals("05", paddedValue);
    }

    @Test
    void padZeroDoesNotPadDoubleDigitValue() {
        String paddedValue = DateUtils.padZero(15);
        assertEquals("15", paddedValue);
    }

    @Test
    void completeDateCompletesYearOnlyDate() {
        String completedDate = DateUtils.completeDate("2023");
        assertEquals("2023-01-01", completedDate);
    }

    @Test
    void completeDateCompletesYearAndMonthDate() {
        String completedDate = DateUtils.completeDate("2023-9");
        assertEquals("2023-09-01", completedDate);
    }

    @Test
    void completeDateReturnsNullForInvalidDate() {
        String completedDate = DateUtils.completeDate("invalid");
        assertNull(completedDate);
    }

    @Test
    void formatAndValidateDateFormatsAndValidatesDate() {
        String formattedDate = DateUtils.formatAndValidateDate("2023-9-5");
        assertEquals("2023-09-05", formattedDate);
    }

    @Test
    void formatAndValidateDateReturnsNullForInvalidDate() {
        String formattedDate = DateUtils.formatAndValidateDate("invalid");
        assertNull(formattedDate);
    }

    @Test
    void formatAndValidateTimeFormatsAndValidatesTime() {
        String formattedTime = DateUtils.formatAndValidateTime("2:30");
        assertEquals("02:30:00", formattedTime);
    }

    @Test
    void formatAndValidateTimeReturnsNullForInvalidTime() {
        String formattedTime = DateUtils.formatAndValidateTime("invalid");
        assertNull(formattedTime);
    }
}