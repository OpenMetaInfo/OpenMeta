package info.openmeta.framework.base.utils;

import info.openmeta.framework.base.constant.StringConstant;
import jakarta.validation.constraints.NotNull;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * String utility class.
 */
@Slf4j
public class StringTools {

    private static final String HTTP_PREFIX = "http:";
    private static final String HTTPS_PREFIX = "https:";

    private StringTools() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Convert List<String> object to comma-separated string
     *
     * @param stringList List<String>
     * @return String
     */
    public static String listToString(List<String> stringList) {
        if (CollectionUtils.isEmpty(stringList)) {
            return StringUtils.EMPTY;
        }
        return String.join(StringConstant.COMMA_SEPARATOR, stringList);
    }

    /**
     * Convert List<Integer> object to comma-separated string
     *
     * @param integerList List<Integer>
     * @return String
     */
    public static String integerListToString(List<Integer> integerList) {
        if (CollectionUtils.isEmpty(integerList)) {
            return StringUtils.EMPTY;
        }
        List<String> stringList = integerList.stream().map(String::valueOf).collect(Collectors.toList());
        return String.join(StringConstant.COMMA_SEPARATOR, stringList);
    }

    /**
     * Convert comma-separated string to List<String> object
     *
     * @param param comma-separated string
     * @return List<String>
     */
    public static List<String> stringToList(String param) {
        if (StringUtils.isNotBlank(param)) {
            return new ArrayList<>(Arrays.asList(param.split(StringConstant.COMMA_SEPARATOR)));
        }
        return Collections.emptyList();
    }

    /**
     * Convert comma-separated string to List<String> object, with all leading and trailing space removed,
     *
     * @param param comma-separated string
     * @return List<String>
     */
    public static List<String> stringTrimToList(String param) {
        if (StringUtils.isNotBlank(param)) {
            String[] split = param.split(StringConstant.COMMA_SEPARATOR);
            return Arrays.stream(split).map(String::trim).collect(Collectors.toList());
        }
        return Collections.emptyList();
    }

    /**
     * Convert comma-separated string to List<Integer> object
     *
     * @param param comma-separated string
     * @return List<Integer>
     */
    public static List<Integer> stringToIntegerList(String param) {
        try {
            if (StringUtils.isNotBlank(param)) {
                List<String> stringList = Arrays.asList(param.split(StringConstant.COMMA_SEPARATOR));
                return stringList.stream().map(Integer::valueOf).collect(Collectors.toList());
            }
        } catch (Exception e) {
            log.warn("An exception occurred while converting the string to an integer list. Param: " + param, e);
        }
        return Collections.emptyList();
    }

    /**
     * Convert comma-separated string to List<Long> object
     *
     * @param param comma-separated string
     * @return List<Long>
     */
    public static List<Long> stringToLongList(String param) {
        try {
            if (StringUtils.isNotBlank(param)) {
                List<String> stringList = Arrays.asList(param.split(StringConstant.COMMA_SEPARATOR));
                return stringList.stream().map(Long::valueOf).collect(Collectors.toList());
            }
        } catch (Exception e) {
            log.warn("An exception occurred while converting the string to a long list. Param: " + param, e);
        }
        return Collections.emptyList();
    }

    /**
     * Replace the http protocol with https. If it is already https, no replacement is made.
     *
     * @param url url
     * @return https url
     */
    public static String httpToHttps(String url) {
        if (StringUtils.isBlank(url)) {
            return url;
        }
        url = url.trim();
        if (url.startsWith(HTTPS_PREFIX)) {
            return url;
        }
        return url.replace(HTTP_PREFIX, HTTPS_PREFIX);
    }

    /**
     * Replace the https protocol with http. If it is already http, no replacement is made.
     *
     * @param url url
     * @return http url
     */
    public static String httpsToHttp(String url) {
        if (StringUtils.isBlank(url)) {
            return url;
        }
        url = url.trim();
        return url.replace(HTTPS_PREFIX, HTTP_PREFIX);
    }

    /**
     * String to underscore naming can be used for attribute name to field name conversion.
     * The number after the letter is not separated.
     * For example,
     *      deptId -> dept_id,
     *      hello123World -> hello123_world,
     *      JSON -> json,
     *      HTTPServer -> http_server
     *
     * @param value string
     * @return lowercase characters separated by underscores
     */
    public static String toUnderscoreCase(String value) {
        if (StringUtils.isBlank(value)) {
            return value;
        }
        StringBuilder result = new StringBuilder();
        final char[] c = value.toCharArray();
        int previousType = Character.getType(c[0]);
        // Convert the first character to lowercase
        result.append(Character.toLowerCase(c[0]));
        for (int pos = 1; pos < c.length; pos++) {
            final int currentType = Character.getType(c[pos]);
            if (currentType == Character.LOWERCASE_LETTER || currentType == Character.DECIMAL_DIGIT_NUMBER) {
                // When the current character is lowercase or a number, add it directly
                result.append(c[pos]);
            } else if (currentType == Character.UPPERCASE_LETTER) {
                if (previousType == Character.LOWERCASE_LETTER || previousType == Character.DECIMAL_DIGIT_NUMBER) {
                    // When the current character is an uppercase letter,
                    // and the previous character is a lowercase letter or number, add an underscore
                    result.append('_');
                } else if (previousType == Character.UPPERCASE_LETTER
                        && (pos + 1 < c.length)
                        && Character.getType(c[pos + 1]) == Character.LOWERCASE_LETTER) {
                    // When the current character is an uppercase letter, the previous character is an uppercase letter,
                    // and the next character is a lowercase letter, add an underscore, such as HTTPServer -> http_server.
                    result.append('_');
                }
                result.append(Character.toLowerCase(c[pos]));
            }
            previousType = currentType;
        }
        return result.toString();
    }

    /**
     * String to all uppercase underscore namings, also known as uppercase snake naming.
     * Can be used to convert the Enum optionCode to the name of the Enum item name,
     * and call the built-in method valueOf() of the Enum to get the Enum item.
     * For example,
     *      ManyToMany → MANY_TO_MANY,
     *      String → STRING,
     *      JSON → JSON,
     *      DateTime → DATE_TIME
     *
     * @param value string
     * @return lowercase characters separated by underscores
     */
    public static String toUpperUnderscoreCase(String value) {
        if (StringUtils.isBlank(value)) {
            return value;
        }
        return StringUtils.join(StringUtils.splitByCharacterTypeCamelCase(value), "_").toUpperCase();
    }

    /**
     * Check whether the model name is valid.
     * Matching a string starting with a capital letter + at least one letter or number.
     * For example,
     *      Dept, User, Order, OrderDetail
     *
     * @param modelName model name
     * @return true / false
     */
    public static boolean isModelName(@NotNull String modelName) {
        String pattern = "^[A-Z][a-zA-Z0-9]+$";
        return Pattern.matches(pattern, modelName);
    }

    /**
     * Check whether the field name is valid,
     * matching a string starting with a lowercase letter + at least one letter or number.
     * For example,
     *      deptId, userId, orderNo, orderDetailId
     *
     * @param fieldName field name
     * @return true / false
     */
    public static boolean isFieldName(@NotNull String fieldName) {
        String pattern = "^[a-z][a-zA-Z0-9]+$";
        return Pattern.matches(pattern, fieldName);
    }

    /**
     * Check whether the string is in underscore naming:
     *      starting with a lowercase letter,
     *      only contains lowercase letters, numbers, and underscores,
     *      and the underscore cannot be continuous or at the end.
     * Suitable for scenarios: database table name, database column name.
     *
     * @param name name
     * @return true / false
     */
    public static boolean isUnderscoreCaseValid(String name) {
        if (name == null || name.endsWith("_") || name.contains("__")) {
            return false;
        }
        String pattern = "^[a-z][a-z0-9_]*$";
        return Pattern.matches(pattern, name);
    }

    /**
     * Remove the last ',' symbol of the non-empty StringBuilder object
     * @param sb StringBuilder object
     */
    public static StringBuilder removeLastComma(StringBuilder sb) {
        Assert.notNull(sb, "String cannot be null!");
        int length = sb.length();
        if (length > 0 && sb.charAt(length - 1) == ',') {
            sb.deleteCharAt(length - 1);
        }
        return sb;
    }

    /**
     * Convert database column name to camel case field name: dept_id -> deptId
     *
     * @param columnName column name
     * @return field name
     */
    public static String toCamelCase(String columnName) {
        if (columnName == null || columnName.isEmpty()) {
            return columnName;
        }
        StringBuilder sb = new StringBuilder(columnName.length());
        boolean toUpper = false;
        for (int i = 0; i < columnName.length(); i++) {
            char c = columnName.charAt(i);
            if (c == '_') {
                toUpper = true;
            } else {
                sb.append(toUpper ? Character.toUpperCase(c) : c);
                toUpper = false;
            }
        }
        return sb.toString();
    }

    /**
     * Convert string to upper camel case, that is, convert hyphen, underscore separated string to camel case.
     * For example,
     *      dept_id → DeptId,
     *      user_name → UserName,
     *      order_no → OrderNo
     *
     * @param str string
     * @return upper camel case string
     */
    public static String toUpperCamelCase(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        StringBuilder sb = new StringBuilder();
        String[] words = str.split("[\\W_]+");
        for (String word : words) {
            if (!word.isEmpty()) {
                sb.append(Character.toUpperCase(word.charAt(0)));
                if (word.length() > 1) {
                    sb.append(word.substring(1).toLowerCase());
                }
            }
        }
        return sb.toString();
    }

    /**
     * Convert the first letter of the string to lowercase
     * @param str string
     * @return string with the first letter in lowercase
     */
    public static String lowerCaseFirstLetter(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return Character.toLowerCase(str.charAt(0)) + (str.length() > 1 ? str.substring(1) : "");
    }

    /**
     * Determine whether the string is an expression, the expression is wrapped in `${}`
     *
     * @param str string
     * @return whether it is a calculation expression
     */
    public static boolean isExpression(String str) {
        return str != null && str.startsWith(StringConstant.FORMULA_PREFIX) && str.endsWith("}");
    }

    /**
     * Determine whether the string is a variable, the variable is wrapped in `#{}`.
     *
     * @param str string
     * @return whether it is a variable parameter
     */
    public static boolean isVariable(String str) {
        return str != null && str.startsWith(StringConstant.VARIABLE_PREFIX) && str.endsWith("}");
    }

    /**
     * Determine whether the string is a reserved field, the reserved field is wrapped in `@{}`.
     *
     * @param str string
     * @return whether it is a reserved field
     */
    public static boolean isReservedField(String str) {
        return str != null && str.startsWith(StringConstant.RESERVED_PREFIX) && str.endsWith("}");
    }

    /**
     * Split the idPath separated by `/` into a set of Long type ids.
     *
     * @param idPath `/` separated string
     * @return id set
     */
    public static Set<Long> splitIdPath(String idPath) {
        if (StringUtils.isBlank(idPath)) {
            return Collections.emptySet();
        }
        String[] ids = idPath.split("/");
        Set<Long> parentIds = new HashSet<>();
        for (String id : ids) {
            parentIds.add(Long.parseLong(id));
        }
        return parentIds;
    }
}
