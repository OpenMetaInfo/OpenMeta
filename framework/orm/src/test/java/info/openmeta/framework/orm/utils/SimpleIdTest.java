package info.openmeta.framework.orm.utils;

import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

@Slf4j
class SimpleIdTest {

    private SimpleId simpleId;

    @BeforeEach
    void setUp() {
        simpleId = SimpleId.getInstance();
    }

    @Test
    void nextIdGeneratesUniqueIds() {
        long id1 = simpleId.nextId();
        long id2 = simpleId.nextId();
        assertNotEquals(id1, id2);
    }

    @Test
    void getNodeIdFromMacReturnsValidNodeId() {
        int nodeId = SimpleId.getNodeIdFromMac();
        assertTrue(nodeId >= 0 && nodeId < SimpleId.NODE_LIMIT);
    }

    @Test
    void getInitialCounterReturnsValidCounter() {
        int counter = SimpleId.getInitialCounter();
        assertTrue(counter >= 0 && counter < SimpleId.NODE_LIMIT);
    }

    @Test
    void getIdDetailsReturnsCorrectDetails() {
        SimpleId simpleId = SimpleId.getInstance();
        long id = simpleId.nextId();
        String details = simpleId.getIdDetails(id);
        assertTrue(details.contains("ID: " + id));
    }

    @Test
    void validateTheMaxTimestamp() {
        long maxId = 9007199254740991L;
        String maxDatetime = "2092-01-19T03:14:07Z";
        assertEquals(SimpleId.getDatetime(maxId), maxDatetime);
    }
}