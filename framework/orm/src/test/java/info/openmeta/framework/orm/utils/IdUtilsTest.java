package info.openmeta.framework.orm.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class IdUtilsTest {

    @Test
    void convertIdToLong() {
        assertNull(IdUtils.convertIdToLong(null));
        assertEquals(1L, IdUtils.convertIdToLong(1));
        assertEquals(1L, IdUtils.convertIdToLong(1));
        assertEquals(1L, IdUtils.convertIdToLong("1"));
        assertEquals(1L, IdUtils.convertIdToLong(1L));
    }
}