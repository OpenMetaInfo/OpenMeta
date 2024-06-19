package info.openmeta.framework.base.utils;

import java.time.LocalDate;

/**
 * Get the zodiac sign based on the birthdate.
 */
public class ZodiacSignUtils {

    // Names of the Zodiac signs, ordered by the Zodiac cycle starting from Aquarius.
    private static final String[] ZODIAC_SIGNS = {
            "Aquarius", "Pisces", "Aries", "Taurus", "Gemini", "Cancer",
            "Leo", "Virgo", "Libra", "Scorpio", "Sagittarius", "Capricorn"
    };

    // The day of the month when the Zodiac sign starts.
    public static final int[] ZODIAC_SIGN_START_DAYS = {
            20, 19, 21, 21, 21, 22,
            23, 23, 23, 23, 22, 22
    };

    /**
     * Returns the Zodiac sign corresponding to the given birthdate.
     */
    public static String getZodiacSign(LocalDate date) {
        int month = date.getMonthValue();
        int day = date.getDayOfMonth();
        if (day < ZODIAC_SIGN_START_DAYS[month]) {
            month = month - 1;
        }
        return ZODIAC_SIGNS[month];
    }
}
