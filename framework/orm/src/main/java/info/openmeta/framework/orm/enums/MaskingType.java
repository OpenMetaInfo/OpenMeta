package info.openmeta.framework.orm.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Masking type:
 *      ALL: default, all characters are replaced with `****`,
 *      NAME: Retain the first and last characters.
 *          If the name contains only two characters, the last character is retained.
 *      EMAIL: Only retain the first 4 characters.
 *      PHONE_NUMBER: replace the last 4 digits with `****`,
 *      ID_NUMBER: Retain the first and last 4 digits and replace other characters with `****`,
 *      CARD_NUMBER: Only retain the last 4 characters.
 */
@Getter
@AllArgsConstructor
public enum MaskingType {
    ALL("All", "Masks all string"),
    NAME("Name", "Masks name"),
    EMAIL("Email", "Masks email"),
    PHONE_NUMBER("PhoneNumber", "Masks Phone Number"),
    ID_NUMBER("IdNumber", "Masks Id Number"),
    CARD_NUMBER("CardNumber", "Masks Card Number");

    @JsonValue
    private final String type;

    private final String description;
}
