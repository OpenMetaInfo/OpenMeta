package info.openmeta.starter.file.excel.handler;

import info.openmeta.framework.orm.meta.MetaField;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class DateHandlerTest {

    private DateHandler dateHandler;

    @BeforeEach
    void setUp() {
        MetaField metaField = new MetaField();
        dateHandler = new DateHandler(metaField);
    }

    @Test
    void handleValueWithValidDateFormat() {
        Object result = dateHandler.handleValue("2024-09-15");
        assertEquals("2024-09-15", result);
    }

    @Test
    void handleValueWithValidDateFormatWithoutSeparators() {
        Object result = dateHandler.handleValue("20240915");
        assertEquals("2024-09-15", result);
    }

    @Test
    void handleValueWithOnlyYear() {
        Object result = dateHandler.handleValue("2024");
        assertEquals("2024-01-01", result);
    }

    @Test
    void handleValueWithYearAndMonth() {
        Object result = dateHandler.handleValue("2024-09");
        assertEquals("2024-09-01", result);
    }

    @Test
    void handleValueWithInvalidDateFormat() {
        Object result = dateHandler.handleValue("2024-13-01");
        assertNull(result);
    }

    @Test
    void handleValueWithEmptyString() {
        Object result = dateHandler.handleValue("");
        assertEquals("", result);
    }

    @Test
    void handleValueWithNull() {
        Object result = dateHandler.handleValue(null);
        assertNull(result);
    }

    @Test
    void handleValueWithNonStringInput() {
        Object result = dateHandler.handleValue(12345);
        assertEquals(12345, result);
    }
}