package info.openmeta.framework.orm.utils;

import info.openmeta.framework.base.utils.Cast;
import info.openmeta.framework.base.utils.SFunction;
import info.openmeta.framework.base.utils.StringTools;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.lang.Nullable;

import java.io.*;
import java.util.*;
import java.util.stream.Collectors;

/**
 * List utils
 */
@Slf4j
public abstract class ListUtils {

    /**
     * Convert object array to a mutable list.
     */
    @SafeVarargs
    public static <T> List<T> of(T... objects) {
        return new ArrayList<>(Arrays.asList(objects));
    }

    /**
     * Get field names list by lambda expression parameters
     *
     * @param lambdaFields field lambda expressions
     * @return field names list
     */
    @SafeVarargs
    public static <T, R> List<String> getLambdaFields(SFunction<T, R>... lambdaFields) {
        return Arrays.stream(lambdaFields).map(LambdaUtils::getAttributeName).collect(Collectors.toList());
    }

    /**
     * All not blank.
     * @param objects the objects
     * @return true or false
     */
    public static boolean allNotBlank(@Nullable Collection<String> objects) {
        return objects != null && objects.stream().noneMatch(StringUtils::isBlank);
    }

    /**
     * All not null
     * @param objects the objects
     * @return true or false
     */
    public static boolean allNotNull(@Nullable Collection<?>... objects) {
        if (objects == null || objects.length == 0) {
            return false;
        }
        for (Collection<?> object : objects) {
            if (object == null || object.isEmpty() || object.contains(null)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Convert string set to underscore case set.
     * @param objects String set
     * @return Set
     */
    public static Set<String> toUnderlineCase(Set<String> objects) {
        return objects.stream().map(StringTools::toUnderscoreCase).collect(Collectors.toSet());
    }

    /**
     * Cut list.
     * @param objects Collection object
     * @param start start index
     * @param end end index, the return value includes the value of this index
     * @return New list
     */
    public static <E> List<E> cut(@Nullable List<E> objects, Integer start, Integer end) {
        if (objects == null) {
            return new ArrayList<>(0);
        }
        if (start < 0 || end >= objects.size()) {
            throw new IndexOutOfBoundsException("Start or end index out of bounds");
        }
        return new ArrayList<>(objects.subList(start, end + 1));
    }

    /**
     * Extract the non-empty string value of the specified field from the List.
     *
     * @param rows data list
     * @param fieldName field name
     * @return the Map structure of data index-value
     */
    public static Map<Integer, String> extractValueIndexMap(List<Map<String, Object>> rows, String fieldName) {
        Map<Integer, String> indexMap = new HashMap<>();
        for (int i = 0; i < rows.size(); i++) {
            Map<String, Object> row = rows.get(i);
            Object value = row.get(fieldName);
            if (value instanceof String && !((String) value).isEmpty()) {
                indexMap.put(i, (String) value);
            }
        }
        return indexMap;
    }

    /**
     * Batch replaces the value of the specified key in List<Map>.
     * For example, replace plaintext to ciphertext, or ciphertext to plaintext.
     *
     * @param rows data list
     * @param targetValue {original value: new value} Map
     * @param key the key to be replaced in the Map of rows
     * @param defaultValue the default value when the original value is null or an empty string
     */
    public static void replaceValueOfMapList(Collection<Map<String, Object>> rows, Map<String, String> targetValue, String key, Object defaultValue) {
        rows.forEach(row -> {
            String originValue = (String) row.get(key);
            if (StringUtils.isNotBlank(originValue)) {
                // Keep the original value when the target dictionary does not have the key
                if (targetValue.containsKey(originValue)) {
                    row.put(key, targetValue.get(originValue));
                }
            } else if (row.containsKey(key)) {
                row.put(key, defaultValue);
            }
        });
    }

    /**
     * Group by key field and value field
     *
     * @param rows data list
     * @param keyField key field, also the key of the group result
     * @param valueField the field to be summarized as the group result
     * @return group result
     */
    public static Map<Object, List<Object>> groupBy(List<Map<String, Object>> rows, String keyField, String valueField) {
        return rows.stream().collect(
                Collectors.groupingBy(
                        row -> row.get(keyField),
                        Collectors.mapping(row -> row.get(valueField), Collectors.toList())
                )
        );
    }

    /**
     * Group by key field value
     *
     * @param rows data list
     * @param keyField key field
     * @return group result, key is the value of the `keyField`
     */
    public static Map<Object, List<Map<String, Object>>> groupBy(List<Map<String, Object>> rows, String keyField) {
        return rows.stream().collect(Collectors.groupingBy(row -> row.get(keyField)));
    }

    /**
     * Deep copy of List<Map<String, Object>> object.
     *
     * @param rows data list
     * @return deep copy result
     */
    public static List<Map<String, Object>> deepCopy(List<Map<String, Object>> rows) {
        List<Map<String, Object>> deepCopyList = new ArrayList<>(rows.size());
        for (Map<String, Object> row : rows) {
            deepCopyList.add(SerializationUtils.clone((Serializable & Map<String, Object>) row));
        }
        return deepCopyList;
    }

    /**
     * Deep copy of List object.
     *
     * @param src source list
     * @return deep copy result
     */
    public static <T> Collection<T> deepCopyList(Collection<T> src) {
        try {
            ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
            ObjectOutputStream out = new ObjectOutputStream(byteOut);
            out.writeObject(src);

            ByteArrayInputStream byteIn = new ByteArrayInputStream(byteOut.toByteArray());
            ObjectInputStream in = new ObjectInputStream(byteIn);
            return Cast.of(in.readObject());
        } catch (Exception e) {
            log.warn("The deepCopyList exception occurs, returning the original collection. Src: " + src, e);
            return src;
        }
    }

    /**
     * Converts data list to table format compatible with Excel or CSV.
     *
     * @param fields the field list responsible for the column headers
     * @param data the list of data rows to be filled into the file
     * @return List of List for table writing
     */
    public static List<List<Object>> convertToTableData(List<String> fields, List<Map<String, Object>> data) {
        return data.stream()
                .map(row -> fields.stream()
                        .map(row::get)
                        .collect(Collectors.toList()))
                .collect(Collectors.toList());
    }

}
