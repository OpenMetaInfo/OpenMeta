package info.openmeta.starter.file.excel.handler;

import info.openmeta.framework.base.exception.ValidationException;
import info.openmeta.framework.base.utils.DateUtils;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.starter.file.dto.ImportFieldDTO;
import org.springframework.util.StringUtils;

/**
 * DateTimeHandler
 * Compatible with the following datetime formats:
 *      YYYY-MM-DD , 2024-09-15, 2024-9-5
 *      YYYY-MM-DD HH:MM, 2024-09-15 12:30, 2024-9-5 1:30
 *      YYYY-MM-DD HH:MM:SS
 *      YYYY/MM/DD HH:MM:SS
 *      YYYY_MM_DD HH:MM:SS
 *      YYYY.MM.DD HH:MM:SS
 */
public class DateTimeHandler extends BaseImportHandler {

    public DateTimeHandler(MetaField metaField, ImportFieldDTO importFieldDTO) {
        super(metaField, importFieldDTO);
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
        if (datetimeStr.contains(" ")) {
            String[] datetimeArray = datetimeStr.split(" ");
            String dateStr = DateUtils.formatAndValidateDate(datetimeArray[0]);
            String timeStr = DateUtils.formatAndValidateTime(datetimeArray[1]);
            if (dateStr != null && timeStr != null) {
                return dateStr + " " + timeStr;
            } else {
                throw new ValidationException("The datetime format is incorrect: `{0}`", datetimeStr);
            }
        } else {
            String dateStr = DateUtils.formatAndValidateDate(datetimeStr);
            if (dateStr != null) {
                return dateStr + " 00:00:00";
            } else {
                throw new ValidationException("The datetime format is incorrect: `{0}`", datetimeStr);
            }
        }
    }

}
