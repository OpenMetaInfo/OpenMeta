package info.openmeta.framework.base.utils;

import java.math.BigInteger;
import java.security.SecureRandom;
import java.util.Base64;
import java.util.Random;


public class RandomUtils {

    /**
     * Generate a random number of specified length
     * @param numDigits number of digits
     * @return random number string
     */
    public static String randomNumber(int numDigits) {
        Assert.isTrue(numDigits > 0, "Number of digits must be positive.");
        Assert.isTrue(numDigits < 19, "Number of digits must be less than 19.");
        // Use BigInteger to generate a random number with the specified number of digits
        BigInteger bigInteger = new BigInteger(numDigits * 5, new Random());
        String result = bigInteger.toString();
        return result.substring(0, numDigits);
    }

    /**
     * Generate a random number of 4 digits
     * @return random number string
     */
    public static String randomNumber4() {
        return randomString(4);
    }

    /**
     * Generate a random string of specified length
     * @param length length of the random string
     * @return random string
     */
    public static String randomString(int length) {
        Assert.isTrue(length > 0, "Length must be greater than 0.");
        byte[] randomBytes = new byte[length];
        new SecureRandom().nextBytes(randomBytes);
        // Convert byte array to URL safe Base64 encoded string
        return Base64.getUrlEncoder().withoutPadding().encodeToString(randomBytes);
    }

    /**
     * Generate a random string of 64 characters
     */
    public static String randomString64() {
        return randomString(64);
    }
}
