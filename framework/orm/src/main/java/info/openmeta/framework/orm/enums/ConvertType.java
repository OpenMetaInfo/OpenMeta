package info.openmeta.framework.orm.enums;

import com.google.common.collect.Sets;

import java.util.Set;

/**
 * Fields need to be formatted: Boolean, Option/MultiOption, ManyToOne/OneToOne.
 * When processing input data, Boolean, Option/MultiOption fields need to be compatible with
 * original values and display values, i.e. DEFAULT, DISPLAY (import scenario).
 */
public enum ConvertType {
    /**
     * Not formatted, original database value, rarely used, such as obtaining original ciphertext data.
     */
    ORIGINAL,
    /**
     * Standard formatting based on field type, handling dynamic calculation and dynamic cascading fields.
     */
    TYPE_CAST,
    /**
     * Enhanced display value, suitable for import and export scenarios,
     * Boolean, Option/MultiOption, ManyToOne/OneToOne fields are processed into display values
     */
    DISPLAY,
    /**
     * Enhanced display value, suitable for front-end display,
     * Option/MultiOption, ManyToOne/OneToOne fields are processed into OptionReference/ModelReference objects.
     */
    REFERENCE;

    public static final Set<ConvertType> EXPAND_TYPES = Sets.immutableEnumSet(DISPLAY, REFERENCE);
}
