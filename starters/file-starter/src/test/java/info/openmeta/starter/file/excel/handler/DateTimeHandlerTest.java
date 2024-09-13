package info.openmeta.starter.file.excel.handler;

import info.openmeta.framework.orm.meta.MetaField;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class DateTimeHandlerTest {

    private DateTimeHandler datetimeHandler;

    @BeforeEach
    void setUp() {
        MetaField metaField = new MetaField();
        datetimeHandler = new DateTimeHandler(metaField);
    }

    @Test
    void handleValueWithValidDateFormat() {
        Object result = datetimeHandler.handleValue("2024-09-15");
        assertEquals("2024-09-15 00:00:00", result);
    }

    @Test
    void handleValueWithValidDateTimeFormat() {
        Object result = datetimeHandler.handleValue("2024-09-15 12:30");
        assertEquals("2024-09-15 12:30:00", result);
    }

    @Test
    void handleValueWithValidDateTimeFormatWithSeconds() {
        Object result = datetimeHandler.handleValue("2024-09-15 12:30:45");
        assertEquals("2024-09-15 12:30:45", result);
    }

    @Test
    void handleValueWithDifferentSeparators() {
        Object result = datetimeHandler.handleValue("2024/09/15 12:30:45");
        assertEquals("2024-09-15 12:30:45", result);
    }

    @Test
    void handleValueWithInvalidDateFormat() {
        Object result = datetimeHandler.handleValue("2024-13-01 12:30:45");
        assertNull(result);
    }

    @Test
    void handleValueWithEmptyString() {
        Object result = datetimeHandler.handleValue("");
        assertEquals("", result);
    }

    @Test
    void handleValueWithNull() {
        Object result = datetimeHandler.handleValue(null);
        assertNull(result);
    }

    @Test
    void handleValueWithNonStringInput() {
        Object result = datetimeHandler.handleValue(12345);
        assertEquals(12345, result);
    }
}