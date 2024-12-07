package info.openmeta.framework.orm.domain;

import info.openmeta.framework.base.utils.JsonMapper;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

@Slf4j
class OrdersTest {

    @Test
    void of() {
        Orders orders = Orders.of("sequence desc, name, job asc, id desc");
        assertEquals(4, orders.getOrderList().size());
        log.info("orders: {}", orders);
    }

    @Test
    void testSerialize() {
        Orders orders = Orders.of("sequence desc, name, job asc, id desc");
        String str = JsonMapper.objectToString(orders);
        Orders newOrders = JsonMapper.stringToObject(str, Orders.class);
        assertEquals(orders.toString(), Orders.of(newOrders.toString()).toString());
    }

    @Test
    void testListDeserialize() {
        Orders orders2 = JsonMapper.stringToObject("[[22], [\"job\", \"asc\"], [\"id\", \"desc\"]]", Orders.class);
        Orders orders1 = Orders.of("22, job asc, id desc");
        assertEquals(orders1.toString(), orders2.toString());
    }

    @Test
    void testDeserialize() {
        Orders orders = JsonMapper.stringToObject("[\"name\", \"ASC\"]", Orders.class);
        assertEquals(1, orders.getOrderList().size());
    }

    @Test
    void testDeserializeList() {
        Orders orders = JsonMapper.stringToObject("[[\"name\", \"ASC\"], [\"code\", \"desc\"]]", Orders.class);
        assertEquals(2, orders.getOrderList().size());
    }
}