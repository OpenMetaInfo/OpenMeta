package info.openmeta.framework.web.model.serializer;

import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.base.utils.JsonMapper;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

@Slf4j
class OrdersDeserializerTest {
    @Data
    static private class ModelOrders {
        private String modelName;
        private Orders orders;
    }

    @Test
    void testSerializer() {
        Orders orders = Orders.of("age desc, name, job asc, id desc");
        ModelOrders mo = new ModelOrders();
        mo.setOrders(orders);
        String json = JsonMapper.objectToString(mo);
        assertNotNull(json);
    }

    /** orders value null */
    @Test
    void testDeserializerSingleNull() {
        String json = "null";
        Orders orders = JsonMapper.stringToObject(json, Orders.class);
        assertNull(orders);
    }

    /**
     * orders value is a string
     * "name, code ASC, id DESC"
     */
    @Test
    void testDeserializerSingleString() {
        String json = "\"name, code ASC, id DESC\"";
        Orders orders = JsonMapper.stringToObject(json, Orders.class);
        assertNotNull(orders);
        log.info(JsonMapper.objectToString(orders));
    }

    /** orders value is null */
    @Test
    void testDeserializerOrdersNull() {
        String json = "{\"modelName\":null,\"orders\": null}";
        ModelOrders mo = JsonMapper.stringToObject(json, ModelOrders.class);
        assertNull(mo.getOrders());
    }

    /**
     * orders value is a string
     * {"modelName":null,"orders": "name, code ASC, id DESC"}
     */
    @Test
    void testDeserializerOrdersString() {
        String json = "{\"modelName\":null,\"orders\": \"name, code ASC, id DESC\"}";
        ModelOrders mo = JsonMapper.stringToObject(json, ModelOrders.class);
        assertNotNull(mo.getOrders());
        log.info(JsonMapper.objectToString(mo));
    }

}