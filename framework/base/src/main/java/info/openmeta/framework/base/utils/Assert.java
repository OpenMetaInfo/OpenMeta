package info.openmeta.framework.base.utils;

import info.openmeta.framework.base.exception.IllegalArgumentException;
import org.apache.commons.lang3.StringUtils;
import org.springframework.lang.Nullable;
import org.springframework.util.CollectionUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;

/**
 * Parameterized Assert, suitable for IllegalArgumentException message templates.
 */
public abstract class Assert {

    /** Throw IllegalArgumentException. */
    private static void throwException(String message, Object... args) {
        throw new IllegalArgumentException(message, args);
    }

    public static void isTrue(Boolean object, String message, Object... args) {
        if (!Boolean.TRUE.equals(object)) {
            throwException(message, args);
        }
    }

    public static void notTrue(Boolean object, String message, Object... args) {
        if (Boolean.TRUE.equals(object)) {
            throwException(message, args);
        }
    }

    public static void isEmpty(@Nullable Collection<?> objects, String message, Object... args) {
        if (!CollectionUtils.isEmpty(objects)) {
            throwException(message, args);
        }
    }

    /** Collection is not empty. */
    public static void notEmpty(@Nullable Collection<?> objects, String message, Object... args) {
        if (CollectionUtils.isEmpty(objects)) {
            throwException(message, args);
        }
    }

    /** Map is not empty. */
    public static void notEmpty(@Nullable Map<?, ?> objects, String message, Object... args) {
        if (CollectionUtils.isEmpty(objects)) {
            throwException(message, args);
        }
    }

    public static void notBlank(@Nullable String s, String message, Object... args) {
        if (StringUtils.isBlank(s)) {
            throwException(message, args);
        }
    }

    /** Collection does not contain null or blank string. */
    public static void allNotBlank(@Nullable Collection<String> objects, String message, Object... args) {
        if (objects == null || objects.isEmpty() || objects.stream().anyMatch(StringUtils::isBlank)) {
            throwException(message, args);
        }
    }

    public static void notNull(@Nullable Object object, String message, Object... args) {
        if (object == null) {
            throwException(message, args);
        }
    }

    /** Object[] does not contain null. */
    public static void allNotNull(@Nullable Object[] objects, String message, Object... args) {
        if (objects == null || Arrays.asList(objects).contains(null)) {
            throwException(message, args);
        }
    }

    /** Collection does not contain null. */
    public static void allNotNull(@Nullable Collection<?> objects, String message, Object... args) {
        if (objects == null || objects.isEmpty() || objects.contains(null)) {
            throwException(message, args);
        }
    }

}
