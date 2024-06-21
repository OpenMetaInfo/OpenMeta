package info.openmeta.framework.orm.domain.serializer;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import info.openmeta.framework.base.exception.JSONException;
import info.openmeta.framework.orm.domain.Filters;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Filters deserialization method, reuse of(String), of(List) methods, compatible with strings and List objects.
 */
public class FiltersDeserializer extends JsonDeserializer<Filters> {

    /**
     * @param p    Parsed used for reading JSON content
     * @param ctxt Context that can be used to access information about
     *             this deserialization activity.
     * @return Deserialized value
     */
    @Override
    public Filters deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
        // Use the Filters.of() method to create a filters object from a JSON string
        JsonToken currentToken = p.currentToken();
        if (currentToken == JsonToken.VALUE_STRING) {
            // If the current token is a string, call the Filters.of() method directly
            String jsonString = p.readValueAs(String.class);
            return Filters.of(jsonString);
        } else if (currentToken == JsonToken.START_ARRAY) {
            // If the current token is an array, parse the entire array into an ArrayList,
            // then call the Filters.of() method
            List<Object> list = p.readValueAs(new TypeReference<ArrayList<Object>>() {});
            return Filters.of(list);
        } else {
            throw new JSONException("The parameter does not support deserialization into a Filters object: {0}", p.readValueAs(Object.class));
        }
    }
}
