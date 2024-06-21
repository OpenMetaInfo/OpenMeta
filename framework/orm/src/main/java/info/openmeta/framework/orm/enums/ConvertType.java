package info.openmeta.framework.orm.enums;

import com.google.common.collect.Sets;

import java.util.Set;

/**
 * Field types need to be formatted: Boolean, Option/MultiOption, ManyToOne/OneToOne.
 * Convert types:
 *      NONE: no formatting is done
 *      DEFAULT: standard formatting based on field type, handling dynamic calculation and dynamic cascading fields,
 *      DISPLAY: enhanced display value, suitable for import and export scenarios,
 *              Boolean, Option/MultiOption, ManyToOne/OneToOne fields are processed into display values,
 *      KEY_AND_DISPLAY: enhanced display value, suitable for front-end display,
 *              Boolean, Option/MultiOption, ManyToOne/OneToOne fields are processed into [key, display value].
 * <p>
 * When processing input data, Boolean, Option/MultiOption fields need to be compatible with
 * original values and display values, i.e. DEFAULT, DISPLAY (import scenario).
 */
public enum ConvertType {
    // No formatting, original database value, rarely used, such as obtaining original ciphertext data
    NONE,
    // Standard formatting based on field type, handling dynamic calculation and dynamic cascading fields
    DEFAULT,
    // Enhanced display value, suitable for import and export scenarios,
    // Boolean, Option/MultiOption, ManyToOne/OneToOne fields are processed into display values
    DISPLAY,
    // Enhanced display value, suitable for front-end display,
    // Boolean, Option/MultiOption, ManyToOne/OneToOne fields are processed into [key, display value]
    KEY_AND_DISPLAY;

    public static final Set<ConvertType> EXPAND_TYPES = Sets.immutableEnumSet(DISPLAY, KEY_AND_DISPLAY);
}
