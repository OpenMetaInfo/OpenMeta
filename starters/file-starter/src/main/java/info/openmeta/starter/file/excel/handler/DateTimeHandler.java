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
            return this.handleDateTimeString(datetimeStr);
        }
        return value;
    }

    /**
     * Handle the datetime string
     * @param datetimeStr The datetime string
     * @return The handled datetime string
     */
    private String handleDateTimeString(String datetimeStr) {
        String[] datetimeArray = datetimeStr.split(" ", 2);
        String dateStr = DateUtils.formatAndValidateDate(datetimeArray[0]);
        String timeStr = datetimeArray.length > 1 ? DateUtils.formatAndValidateTime(datetimeArray[1]) : "00:00:00";
        if (dateStr == null || timeStr == null) {
            throw new ValidationException("The datetime field `{0}` is incorrect: `{1}`", labelName, datetimeStr);
        }
        return dateStr + " " + timeStr;
    }

}
