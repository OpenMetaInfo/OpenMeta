package info.openmeta.starter.file.excel.handler;

import info.openmeta.framework.orm.meta.MetaField;
import org.springframework.util.StringUtils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * DateHandler
 * Compatible with the following date formats:
 *      YYYY-MM-DD
 *      YYYY/MM/DD
 *      YYYY_MM_DD
 *      YYYY.MM.DD
 *      YYYYMMDD
 *      YYYY    =  YYYY-01-01
 *      YYYY-MM =  YYYY-MM-01
 *      YYYY/MM =  YYYY-MM-01
 *      YYYY_MM =  YYYY-MM-01
 *      YYYY.MM =  YYYY-MM-01
 */
public class DateHandler extends BaseImportHandler {

    public DateHandler(MetaField metaField) {
        super(metaField);
    }

    /**
     * Handle the value of the date field
     * @param value The value
     * @return The handled value
     */
    public Object handleValue(Object value) {
        if (value instanceof String dateStr && StringUtils.hasText(dateStr)) {
            // Convert the date string to a standard format
            dateStr = dateStr.trim().toLowerCase();
            return handleDateString(dateStr);
        }
        return value;
    }

    /**
     * Handle the date string
     * @param dateStr The date string
     * @return The handled date string
     */
    private static String handleDateString(String dateStr) {
        // replace all separators with hyphen
        dateStr = dateStr.replaceAll("[/_.]", "-");
        String regex = "^((1[8-9]\\d{2})|([2-9]\\d{3}))-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$";
        if (dateStr.length() == 8) {
            // Process 8-digit dates without separators, such as 20240915 to 2024-09-15
            dateStr = dateStr.substring(0, 4) + "-" + dateStr.substring(4, 6) + "-" + dateStr.substring(6);
        } else if (dateStr.length() == 4) {
            // Compatible with only the year, such as 2024 to 2024-01-01
            dateStr += "-01-01";
        } else if (dateStr.length() == 7) {
            // Compatible with the year-month, such as 2024-09 to 2024-09-01
            dateStr += "-01";
        }
        // Use regex to verify the date format
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(dateStr);
        if (matcher.matches()) {
            return dateStr;
        } else {
            return null;
        }
    }

}
