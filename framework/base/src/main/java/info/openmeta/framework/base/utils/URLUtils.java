package info.openmeta.framework.base.utils;

import org.springframework.util.AntPathMatcher;
import org.springframework.util.StringUtils;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * URL utility class
 */
public class URLUtils {

    private static final AntPathMatcher matcher = new AntPathMatcher();

    /**
     * Concatenate multiple URL fragments into a complete URI without parameters
     */
    public static String buildUrl(String... clauses) {
        if (clauses.length == 0) {
            return "";
        }
        String uri = String.join("/", clauses);
        // Remove redundant slashes
        return uri.replaceAll("(?<!:)//+", "/");
    }

    /**
     * Add parameters to the URL.
     * Compatible with URL already containing parameters.
     */
    public static String addParamsToUrl(String url, Map<String, Object> parameters) {
        if (!StringUtils.hasText(url) || parameters == null || parameters.isEmpty()) {
            return url;
        }
        var queryParams = parameters.entrySet().stream()
                .map(entry -> encodeParam(entry.getKey()) + "=" + encodeParam(entry.getValue().toString()))
                .collect(Collectors.joining("&"));
        if (url.endsWith("?")) {
            return url + queryParams;
        } else if (url.contains("?")) {
            return url + "&" + queryParams;
        } else {
            return url + "?" + queryParams;
        }
    }

    /**
     * Encodes a query parameter using UTF-8.
     */
    public static String encodeParam(String param) {
        return URLEncoder.encode(param, StandardCharsets.UTF_8);
    }

    /**
     * Check if the URI matches the pattern
     * @param pattern pattern
     * @param uri URI
     * @return true if the URI matches the pattern
     */
    public static boolean matchUri(String pattern, String uri) {
        return matcher.match(pattern, uri);
    }

    /**
     * Check if the URI matches any of the patterns
     * @param patterns patterns
     * @param uri URI
     * @return true if the URI matches any of the patterns
     */
    public static boolean matchUri(List<String> patterns, String uri) {
        for (String pattern : patterns) {
            if (matchUri(pattern, uri)) {
                return true;
            }
        }
        return false;
    }
}
