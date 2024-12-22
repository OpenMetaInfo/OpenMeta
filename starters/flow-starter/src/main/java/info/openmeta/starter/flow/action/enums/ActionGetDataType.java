package info.openmeta.starter.flow.action.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import com.google.common.collect.Sets;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Set;

/**
 * Action Get Data Type
 */
@Getter
@AllArgsConstructor
public enum ActionGetDataType {

    // Get multiple rows of data, support sorting, but do not allow to exceed the maximum batch limit
    MULTI_ROWS("MultiRows", "Multi Rows"),
    // Read a single row of data, support sorting, but the limit is 1
    SINGLE_ROW("SingleRow", "Single Row"),
    // Read the value of a single field, support sorting when querying data, but the limit is 1
    ONE_FIELD_VALUE("OneFieldValue", "The value of a single field"),
    // Read the value list of a single field for multiple rows, support sorting when querying data,
    // but do not allow to exceed the maximum batch limit
    ONE_FIELD_VALUES("OneFieldValues", "The value list of a single field for multiple rows"),
    // Determine whether the data exists
    EXIST("Exist", "Whether the data exists"),
    // Count the number of rows that meet the filter conditions
    COUNT("Count", "Count"),;

    @JsonValue
    private final String type;
    private final String description;

    // The types of single row data
    public static final Set<ActionGetDataType> SINGLE_ROW_TYPES = Sets.immutableEnumSet(SINGLE_ROW, ONE_FIELD_VALUE);
    // The types of multiple row data
    public static final Set<ActionGetDataType> MULTI_ROW_TYPES = Sets.immutableEnumSet(MULTI_ROWS, ONE_FIELD_VALUES);
    // Types that involve using count to count
    public static final Set<ActionGetDataType> COUNT_TYPES = Sets.immutableEnumSet(EXIST, COUNT);
}
