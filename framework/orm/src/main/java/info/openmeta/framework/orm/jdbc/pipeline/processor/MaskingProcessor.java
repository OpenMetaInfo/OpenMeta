package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.constant.StringConstant;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.orm.enums.MaskingType;
import info.openmeta.framework.orm.meta.MetaField;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.Map;

/**
 * Masking field processor
 */
public class MaskingProcessor extends StringProcessor {

    /**
     * Field processor object constructor
     *
     * @param metaField field metadata object
     */
    public MaskingProcessor(MetaField metaField) {
        super(metaField);
    }

    /**
     * Batch processing of output rows
     *
     * @param rows List of rows to be processed
     */
    @Override
    public void batchProcessOutputRows(List<Map<String, Object>> rows) {
        if (ContextHolder.getContext().isDataMask()) {
            rows.forEach(row -> row.put(fieldName, maskingFieldValue(row.get(fieldName))));
        }
    }

    /**
     * Masking the field value.
     *
     * @param fieldValue original field value
     * @return Masked field value
     */
    public String maskingFieldValue(Object fieldValue) {
        String value = (String) fieldValue;
        if (StringUtils.isBlank(value)) {
            value = "";
        } else if (MaskingType.NAME.equals(metaField.getMaskingType())) {
            value = maskingName(value);
        } else if (MaskingType.EMAIL.equals(metaField.getMaskingType())) {
            value = maskingEmail(value);
        } else if (MaskingType.PHONE_NUMBER.equals(metaField.getMaskingType())) {
            value = maskingPhoneNumber(value);
        } else if (MaskingType.ID_NUMBER.equals(metaField.getMaskingType())) {
            value = maskingIdNumber(value);
        }  else if (MaskingType.CARD_NUMBER.equals(metaField.getMaskingType())) {
            value = maskingCardNumber(value);
        } else {
            // Default is to replace all with MASKING_SYMBOL
            value = StringConstant.MASKING_SYMBOL;
        }
        return value;
    }

    /**
     * Masking name.
     * Retain the first and last characters, when the name has only 2 characters, retain the last character.
     *
     * @param name name
     * @return masked name
     */
    public static String maskingName(String name) {
        if (name == null || name.length() < 2) {
            return name;
        } else if (name.length() == 2) {
            return StringConstant.MASKING_SYMBOL + name.substring(name.length() - 1);
        } else {
            return name.charAt(0) + StringConstant.MASKING_SYMBOL + name.substring(name.length() - 1);
        }
    }

    /**
     * Masking email, retain only the first 4 characters.
     *
     * @param email email
     * @return masked email
     */
    public static String maskingEmail(String email) {
        if (email != null && email.length() > 4) {
            email = email.substring(0, 4) + StringConstant.MASKING_SYMBOL;
        }
        return email;
    }

    /**
     * Masking phone number, retain only the last 4 characters.
     *
     * @param phoneNumber phone number
     * @return masked phone number
     */
    public static String maskingPhoneNumber(String phoneNumber) {
        if (phoneNumber == null || phoneNumber.length() < 5) {
            return phoneNumber;
        } else {
            return phoneNumber.substring(0, phoneNumber.length() - 4) + StringConstant.MASKING_SYMBOL;
        }
    }

    /**
     * Masking ID number, retain the first and last 4 characters of the ID number,
     * and replace the rest with ****.
     *
     * @param number ID number
     * @return masked ID number
     */
    public static String maskingIdNumber(String number) {
        if (number != null && number.length() > 8) {
            number = number.substring(0, 4) + StringConstant.MASKING_SYMBOL + number.substring(number.length() - 4);
        } else if (StringUtils.isNotBlank(number)) {
            number = StringConstant.MASKING_SYMBOL;
        }
        return number;
    }

    /**
     * Masking bank card number, retain only the last 4 characters of the bank card number,
     * and replace the rest with ****.
     *
     * @param number bank card number
     * @return masked bank card number
     */
    public static String maskingCardNumber(String number) {
        if (number != null && number.length() > 4) {
            number = StringConstant.MASKING_SYMBOL + number.substring(number.length() - 4);
        } else if (StringUtils.isNotBlank(number)) {
            number = StringConstant.MASKING_SYMBOL;
        }
        return number;
    }
}
