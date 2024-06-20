package info.openmeta.framework.base.i18n;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = I18nJSONLoader.class)
class I18nTest {

    @Test
    void get() {
        String trans = I18n.get("Hello world!");
        Assertions.assertNotNull(trans);
    }

    @Test
    void testGet() {
    }
}