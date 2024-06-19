package info.openmeta.framework.base.utils;

import java.util.UUID;

/**
 * UUID utility class
 * 1. Generate UUID string without dashes
 * 2. Generate UUID string with maximum length of 22.
 */
public class UUIDUtils {

    private static final char[] BASE_62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray();

    /**
     * Generate UUID string, remove the '-'.
     * The length of the string is 32.
     */
    public static String uuidWithoutDash() {
        UUID uuid = UUID.randomUUID();
        return uuid.toString().replace("-", "");
    }

    /**
     * Generate UUID string in 62 base.
     * The maximum length of the string is 22.
     */
    public static String short22UUID() {
        UUID uuid = UUID.randomUUID();
        long mostSigBits = uuid.getMostSignificantBits();
        long leastSigBits = uuid.getLeastSignificantBits();
        return longToBase62(mostSigBits) + longToBase62(leastSigBits);
    }

    /**
     * Convert long value to 62 base string
     * @param value long value
     * @return 62 base string
     */
    private static String longToBase62(long value) {
        StringBuilder sb = new StringBuilder();
        do {
            int digit = (int)(value % 62);
            // Convert negative number to positive number
            digit = Math.abs(digit);
            sb.append(BASE_62[digit]);
            value /= 62;
        } while (value != 0);
        return sb.toString();
    }
}
