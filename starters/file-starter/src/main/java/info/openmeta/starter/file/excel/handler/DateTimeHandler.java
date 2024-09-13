package info.openmeta.starter.file.excel.handler;

import info.openmeta.framework.orm.meta.MetaField;
import org.springframework.util.StringUtils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * DateTimeHandler
 * Compatible with the following datetime formats:
 *      YYYY-MM-DD
 *      YYYY-MM-DD HH:MM
 *      YYYY-MM-DD HH:MM:SS
 *      YYYY/MM/DD HH:MM:SS
 *      YYYY_MM_DD HH:MM:SS
 *      YYYY.MM.DD HH:MM:SS
 */
public class DateTimeHandler extends BaseImportHandler {

    public DateTimeHandler(MetaField metaField) {
        super(metaField);
    }

    /**
     * Handle the value of the datetime field
     * @param value The value
     * @return The handled value
     */
    public Object handleValue(Object value) {
        if (value instanceof String datetimeStr && StringUtils.hasText(datetimeStr)) {
            // Convert the date string to a standard format
            datetimeStr = datetimeStr.trim().toLowerCase();
            return handleDateTimeString(datetimeStr);
        }
        return value;
    }

    /**
     * Handle the datetime string
     * @param datetimeStr The datetime string
     * @return The handled datetime string
     */
    private static String handleDateTimeString(String datetimeStr) {
        // replace all separators with hyphen
        datetimeStr = datetimeStr.replaceAll("[/_.]", "-");
        String regex = "^((1[8-9]\\d{2})|([2-9]\\d{3}))-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01]) ([0-1]?[0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9])$";
        if (datetimeStr.length() == 10) {
            // Compatible with the date format, such as 2024-09-15 to 2024-09-15 00:00:00
            datetimeStr += " 00:00:00";
        } else if (datetimeStr.length() == 16) {
            // Compatible with the date and time format, such as 2024-09-15 12:30 to 2024-09-15 12:30:00
            datetimeStr += ":00";
        }
        // Use regex to verify the date format
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(datetimeStr);
        if (matcher.matches()) {
            return datetimeStr;
        } else {
            return null;
        }
    }

}
