package info.openmeta.framework.base.utils;

/**
 * Type casting utility, to avoid unchecked type casting warnings.
 */
public abstract class Cast {

    @SuppressWarnings("unchecked")
    public static <T> T of(Object obj) {
        return (T) obj;
    }
}
