package info.openmeta.framework.orm.domain.serializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import info.openmeta.framework.orm.domain.Orders;

import java.io.IOException;

/**
 * Serialize an Orders object into a JSON string.
 */
public class OrdersSerializer extends JsonSerializer<Orders> {

    /**
     * Method that can be called to ask implementation to serialize
     * values of type this serializer handles.
     * @param orders       Value to serialize; can <b>not</b> be null.
     * @param gen         Generator used to output resulting Json content
     * @param serializers Provider that can be used to get serializers for
     *                    serializing Objects value contains, if any.
     */
    @Override
    public void serialize(Orders orders, JsonGenerator gen, SerializerProvider serializers) throws IOException {
        // Generate a JSON string in the form of [["field1", "ASC"], ["field2", "DESC"]] using Orders' orderList
        gen.writeObject(orders.getOrderList());
    }
}
