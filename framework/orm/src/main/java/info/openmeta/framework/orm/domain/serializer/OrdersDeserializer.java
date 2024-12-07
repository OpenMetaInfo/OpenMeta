package info.openmeta.framework.orm.domain.serializer;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.orm.domain.Orders;

import java.io.IOException;
import java.util.List;

/**
 * Deserialize a string list into an Orders object.
 */
public class OrdersDeserializer extends JsonDeserializer<Orders> {

    /**
     * @param p    Parsed used for reading JSON content
     * @param ctxt Context that can be used to access information about
     *             this deserialization activity.
     * @return Deserialized value
     */
    @Override
    public Orders deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
        JsonToken currentToken = p.currentToken();
        if (JsonToken.VALUE_STRING.equals(currentToken)) {
            // Use the Orders.of() method to create an Orders object from a JSON string
            String jsonString = p.readValueAs(String.class);
            return Orders.of(jsonString);
        } else if (JsonToken.START_ARRAY.equals(currentToken)) {
            // If the current token is an array, parse the entire array into an ArrayList, then call the Orders.of() method.
            List<Object> list = p.readValueAs(new TypeReference<List<Object>>() {});
            return Orders.of(list);
        } else {
            throw new IllegalArgumentException("Sorting parameters do not support deserialization into Orders objects: {0}", p.readValueAs(Object.class));
        }
    }
}
