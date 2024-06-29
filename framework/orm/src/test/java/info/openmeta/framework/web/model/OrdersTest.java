package info.openmeta.framework.web.model;

import info.openmeta.framework.orm.domain.Orders;
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
}