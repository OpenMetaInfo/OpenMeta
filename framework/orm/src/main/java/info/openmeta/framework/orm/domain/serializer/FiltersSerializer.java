package info.openmeta.framework.orm.domain.serializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import info.openmeta.framework.orm.domain.Filters;

import java.io.IOException;

/**
 * Filters serialization method is the same as toString()
 */
public class FiltersSerializer extends JsonSerializer<Filters> {

    /**
     * Method that can be called to ask implementation to serialize
     * values of type this serializer handles.
     * @param filters     Value to serialize; can <b>not</b> be null.
     * @param gen         Generator used to output resulting Json content
     * @param serializers Provider that can be used to get serializers for
     *                    serializing Objects value contains, if any.
     */
    @Override
    public void serialize(Filters filters, JsonGenerator gen, SerializerProvider serializers) throws IOException {
        // Use the Filters.toString() method to generate a JSON string
        gen.writeRawValue(filters.toString());
    }
}
