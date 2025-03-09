package info.openmeta.framework.base.utils;

import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

@Slf4j
class StringToolsTest {

    @Test
    void toUnderscoreCase() {
        Assertions.assertEquals("http_server", StringTools.toUnderscoreCase("HTTPServer"));
        Assertions.assertEquals("json", StringTools.toUnderscoreCase("JSON"));
        Assertions.assertEquals("json", StringTools.toUnderscoreCase("Json"));
        Assertions.assertEquals("s9001_test", StringTools.toUnderscoreCase("S9001Test"));
    }

}