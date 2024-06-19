package info.openmeta.framework.base.utils;


import org.apache.commons.text.StringSubstitutor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Placeholder Utils
 */
public class PlaceholderUtils {
    /** The prefix of placeholder */
    private static final String PLACEHOLDER_PREFIX = "#{";

    /** The suffix of placeholder */
    private static final String PLACEHOLDER_SUFFIX = "}";

    /**
     * Extract the placeholder variable list in the text.
     * Such as extracting name from #{name}
     * @param text text
     * @return placeholder variable list
     */
    public static List<String> extractVariables(String text) {
        List<String> variables = new ArrayList<>();
        // Escape the delimiter
        String escapedPrefixDelimiter = Pattern.quote(PLACEHOLDER_PREFIX);
        String escapedSuffixDelimiter = Pattern.quote(PLACEHOLDER_SUFFIX);
        String regex = escapedPrefixDelimiter + "(.+?)" + escapedSuffixDelimiter;
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(text);
        // Find all the placeholders
        while (matcher.find()) {
            variables.add(matcher.group(1));
        }
        return variables;
    }

    /**
     * Replace a single placeholder variable in the text
     *
     * @param text text
     * @param placeholder placeholder variable
     * @param value value
     * @return replaced text
     */
    public static String replacePlaceholder(String text, String placeholder, String value) {
        String regex = PLACEHOLDER_PREFIX + placeholder + PLACEHOLDER_SUFFIX;
        return text.replaceAll(regex, value);
    }

    /**
     * Replace multiple placeholder variables in the text
     * @param text text
     * @param placeholderMap placeholder variable map
     * @return replaced text
     */
    public static String replacePlaceholders(String text, Map<String, Object> placeholderMap) {
        StringSubstitutor sub = new StringSubstitutor(placeholderMap, PLACEHOLDER_PREFIX, PLACEHOLDER_SUFFIX);
        // Disable recursive and nested placeholders
        sub.setEnableSubstitutionInVariables(false);
        return sub.replace(text);
    }
}
