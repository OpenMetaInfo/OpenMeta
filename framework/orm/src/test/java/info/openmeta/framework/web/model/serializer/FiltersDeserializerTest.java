package info.openmeta.framework.web.model.serializer;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.base.utils.JsonMapper;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

@Slf4j
class FiltersDeserializerTest {
    @Data
    static private class ModelFilters {
        private String modelName;
        private Filters filters;
    }

    @Test
    void testSerializer() {
        Filters filters = Filters.eq("name", "demo");
        ModelFilters framework = new ModelFilters();
        framework.setFilters(filters);
        String json = JsonMapper.objectToString(framework);
        log.info("json: {}", json);
        assertNotNull(json);
    }

    /** null filters value */
    @Test
    void testDeserializerSingleNull() {
        String json = "null";
        Filters filters = JsonMapper.stringToObject(json, Filters.class);
        assertNull(filters);
    }

    /**
     * a string of LEAF filters: "[\"name\",\"=\",\"demo\"]"
     */
    @Test
    void testDeserializerSingleString() {
        String json = "\"[\\\"name\\\",\\\"=\\\",\\\"demo\\\"]\"";
        Filters filters = JsonMapper.stringToObject(json, Filters.class);
        assertNotNull(filters);
        log.info(filters.toString());
    }

    /**
     * json string of LEAF filters: ["name","=","demo"]
     */
    @Test
    void testDeserializerSingleList() {
        String json = "[\"name\",\"=\",\"demo\"]";
        Filters filters = JsonMapper.stringToObject(json, Filters.class);
        assertNotNull(filters);
        log.info(filters.toString());
    }

    /** filters value is null */
    @Test
    void testDeserializerFiltersNull() {
        String json = "{\"modelName\":null,\"filters\": null}";
        ModelFilters framework = JsonMapper.stringToObject(json, ModelFilters.class);
        assertNull(framework.getFilters());
    }

    /**
     * filters value is a string
     * {"modelName":null,"filters":"[\"name\",\"=\",\"demo\"]"}
     */
    @Test
    void testDeserializerFiltersString() {
        String json = "{\"modelName\":null,\"filters\":\"[\\\"name\\\",\\\"=\\\",\\\"demo\\\"]\"}";
        ModelFilters framework = JsonMapper.stringToObject(json, ModelFilters.class);
        assertNotNull(framework.getFilters());
        log.info(framework.getFilters().toString());
    }

    /**
     * filters value is a list object.
     * {"modelName":null,"filters": ["name","=","demo"]}
     */
    @Test
    void testDeserializerFiltersList() {
        String json = "{\"modelName\":null,\"filters\": [\"name\",\"=\",\"demo\"]}";
        ModelFilters framework = JsonMapper.stringToObject(json, ModelFilters.class);
        assertNotNull(framework.getFilters());
        log.info(framework.getFilters().toString());
    }
}