package info.openmeta.framework.orm.domain.serializer;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.orm.domain.SubQueries;
import org.springframework.util.StringUtils;

import java.io.IOException;

/**
 * Deserialize a JSON string into an SubQueries object.
 */
public class SubQueriesDeserializer extends JsonDeserializer<SubQueries> {

    /**
     * @param p    Parsed used for reading JSON content
     * @param ctxt Context that can be used to access information about
     *             this deserialization activity.
     * @return Deserialized value
     */
    @Override
    public SubQueries deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
        JsonToken currentToken = p.currentToken();
        if (JsonToken.VALUE_STRING.equals(currentToken)) {
            String jsonString = p.readValueAs(String.class);
            if (StringUtils.hasText(jsonString)) {
                return JsonMapper.stringToObject(jsonString, SubQueries.class);
            }
        }
        return null;
    }
}
