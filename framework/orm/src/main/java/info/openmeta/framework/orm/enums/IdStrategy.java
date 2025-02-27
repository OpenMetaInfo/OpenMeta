package info.openmeta.framework.orm.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * ID generation strategy.
 */
@Getter
@AllArgsConstructor
public enum IdStrategy {
    // Database Auto-increment ID: 64-bit numeric ID, as default
    DB_AUTO_ID("DbAutoID", "DB Auto-increment ID"),
    /**
     * Unique Lexicographically Sortable Identifier: 128-bit, with 48-bit timestamp and 80-bit random value
     * 26-character string with 10-character timestamp and 16-character random value.
     * ULID is designed to be used as a primary key in distributed systems.
     * More secure than TSID_STRING.
     */
    ULID("ULID", "ULID"),
    /**
     * Numeric time trending Unique Identifier (Combined SnowFlakeID and ULID): 64-bit, with 42-bit timestamp,
     * 10-bit server ID and 12-bit sequence number by default.
     */
    TSID_LONG("TSIDLong", "Long Time-Sorted ID"),
    /**
     * 13-character string TSID, less secure than ULID
     * Suitable for small-scale systems.
     */
    TSID_STRING("TSIDString", "String Time-Sorted ID"),
    // 16-digit numeric Long id
    SIMPLE_ID("SimpleID", "Simple 16-digit Long ID"),
    // Standard UUID: 128-bit, 32-character hexadecimal string
    UUID("UUID", "UUID"),
    // External ID: external input ID
    EXTERNAL_ID("ExternalID", "External ID");

    @JsonValue
    private final String type;

    private final String name;

}
