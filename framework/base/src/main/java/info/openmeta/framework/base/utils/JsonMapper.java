package info.openmeta.framework.base.utils;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import info.openmeta.framework.base.exception.JSONException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * JSON Object Mapper
 */
public class JsonMapper {
    private static ObjectMapper mapper;

    /**
     * Get ObjectMapper instance from the Spring Context.
     * If the Spring Context is not available, create a new ObjectMapper instance.
     * @return ObjectMapper instance
     */
    public static ObjectMapper getMapper() {
        if (mapper != null) {
            return mapper;
        } else if (SpringContextUtils.existApplicationContext()) {
            mapper = SpringContextUtils.getBeanByClass(ObjectMapper.class);
        } else {
            return new ObjectMapper().registerModule(new JavaTimeModule());
        }
        return mapper;
    }

    /**
     * Object to JSON string
     * @param object object instance
     * @return JSON String
     */
    public static String objectToString(Object object) {
        try {
            return getMapper().writeValueAsString(object);
        } catch (JsonProcessingException e) {
            throw new JSONException("JSON serialization failure: {0}", object.toString(), e);
        }
    }

    /**
     * Object to string without null properties.
     * @param object object instance
     * @return JSON String
     */
    public static String objectToStringIgnoreNull(Object object) {
        try {
            ObjectMapper ignoreNullMapper = getMapper().copy().setSerializationInclusion(JsonInclude.Include.NON_NULL);
            return ignoreNullMapper.writeValueAsString(object);
        } catch (JsonProcessingException e) {
            throw new JSONException("JSON serialization failure: {0}", object.toString(), e);
        }
    }

    /**
     * String to object install
     * @param json JSON string
     * @param tClass object type
     * @return object instance
     */
    public static <T> T stringToObject(String json, Class<T> tClass) {
        try {
            return getMapper().readValue(json, tClass);
        } catch (JsonProcessingException e) {
            throw new JSONException("JSON deserialization failure: {0}", json, e);
        }
    }

    /**
     * String to Map or List Object
     * @param json JSON string
     * @param valueTypeRef: such as `new TypeReference<List<Object>>() {}` or `new TypeReference<Map<String, Object>>() {}`
     * @return Map or List object
     */
    public static <T> T stringToObject(String json, TypeReference<T> valueTypeRef) {
        try {
            return getMapper().readValue(json, valueTypeRef);
        } catch (JsonProcessingException e) {
            throw new JSONException("JSON string deserializable failure: {0} ", LogUtils.splitLog(json), e);
        }
    }

    /**
     * Object to JsonNode.
     * @param object object
     * @return JsonNode
     */
    public static JsonNode objectToJsonNode(Object object) {
        try {
            return getMapper().readValue(getMapper().writeValueAsString(object), JsonNode.class);
        } catch (JsonProcessingException e) {
            throw new JSONException(e.getMessage(), e);
        }
    }

    /**
     * JsonNode to Map or List Object
     * @param jsonNode JsonNode
     * @param valueTypeRef: such as `new TypeReference<List<Object>>() {}` or `new TypeReference<Map<String, Object>>() {}`
     * @return Map or List object
     */
    public static <T> T jsonNodeToObject(JsonNode jsonNode, TypeReference<T> valueTypeRef) {
        try {
            JsonParser jsonParser = getMapper().treeAsTokens(jsonNode);
            return getMapper().readValue(jsonParser, valueTypeRef);
        } catch (IOException e) {
            throw new JSONException(e.getMessage(), e);
        }
    }

    /**
     * JsonNode to List, Map or String.
     * @param jsonNode JsonNode
     * @return List, Map or String object
     */
    public static Object jsonNodeToObject(JsonNode jsonNode) {
        if (null == jsonNode) {
            return null;
        }
        if (jsonNode.isArray()) {
            return getMapper().convertValue(jsonNode, ArrayList.class);
        } else if (jsonNode.isObject()) {
            return getMapper().convertValue(jsonNode, Map.class);
        } else {
            return jsonNode.asText();
        }
    }

    /**
     * JsonNode to specified type object.
     * @param jsonNode JsonNode
     * @return object instance
     */
    public static <T> T jsonNodeToObject(JsonNode jsonNode, Class<T> tClass) {
        try {
            return jsonNode == null ? null : getMapper().treeToValue(jsonNode, tClass);
        } catch (JsonProcessingException e) {
            throw new JSONException(e.getMessage(), e);
        }
    }

    /**
     * JsonNode to List<Long>
     * @param jsonNode JsonNode
     * @return List<Long> object
     */
    public static List<Long> jsonNodeToLongList(JsonNode jsonNode) {
        if (null == jsonNode) {
            return null;
        }
        if (jsonNode.isArray()) {
            return getMapper().convertValue(jsonNode, new TypeReference<>() {
            });
        }
        return null;
    }


    /**
     * JsonNode to List<String>
     * @param jsonNode JsonNode
     * @return List<String> object
     */
    public static List<String> jsonNodeToStringList(JsonNode jsonNode) {
        if (null == jsonNode) {
            return null;
        }
        if (jsonNode.isArray()) {
            return getMapper().convertValue(jsonNode, new TypeReference<>() {
            });
        }
        return null;
    }

    /**
     * JsonNode to Map<String, Object>
     * @param jsonNode JsonNode
     * @return Map<String, Object>
     */
    public static Map<String, Object> jsonNodeToMap(JsonNode jsonNode) {
        if (null == jsonNode) {
            return null;
        }
        return jsonNodeToObject(jsonNode, new TypeReference<>() {});
    }

}
