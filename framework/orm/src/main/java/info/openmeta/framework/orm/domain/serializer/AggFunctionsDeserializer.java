package info.openmeta.framework.orm.domain.serializer;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.domain.AggFunctions;
import info.openmeta.framework.orm.enums.AggFunctionType;

import java.io.IOException;

/**
 * Deserialize a string list into an AggFunctions object.
 * Support two-layer lists, [["SUM", "amount"], ["COUNT", "id"]],
 * the inner list can be two elements of function name and field name.
 * Compatible with single-layer lists, ["SUM", "amount"].
 * The field alias of the result uses the camel string spliced by the function name and field name, such as `sumAmount`.
 */
public class AggFunctionsDeserializer extends JsonDeserializer<AggFunctions> {

    /**
     * @param p    Parsed used for reading JSON content
     * @param ctxt Context that can be used to access information about
     *             this deserialization activity.
     * @return Deserialized value
     */
    @Override
    public AggFunctions deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
        JsonNode node = p.getCodec().readTree(p);
        // Determine whether it is a single-layer array or a double-layer array
        if (node.isArray()) {
            AggFunctions aggFunctions = new AggFunctions();
            for (JsonNode innerNode : node) {
                if (innerNode.isArray()) {
                    // double-layer array
                    this.handleAggFunctions(aggFunctions, innerNode);
                } else {
                    // single-layer array, in this case we only have one aggregation element, so exit the loop directly.
                    this.handleAggFunctions(aggFunctions, node);
                    break;
                }
            }
            return aggFunctions;
        } else {
            throw new IllegalArgumentException("Parameter deserialization failed: {0}", node.asText());
        }
    }

    /**
     * Handle the aggregation function node.
     * Compatible with single-layer lists, ["SUM", "amount"], and double-layer lists, [["SUM", "amount"], ["COUNT", "id"]].
     * The optional field alias of the result uses the camel string spliced by the function name and field name,
     * such as `sumAmount`.
     *
     * @param aggFunctions aggregation functions
     * @param node         aggregation function node
     */
    private void handleAggFunctions(AggFunctions aggFunctions, JsonNode node) {
        Assert.isTrue(node.size() == 2 || node.size() == 3,
                "The format of aggregation function is incorrect: {0}", node.asText());
        AggFunctionType type = AggFunctionType.of(node.get(0).asText());
        String field = node.get(1).asText();
        if (node.size() == 3) {
            String alias = node.get(2).asText();
            aggFunctions.add(type, field, alias);
        } else {
            aggFunctions.add(type, field);
        }
    }
}
