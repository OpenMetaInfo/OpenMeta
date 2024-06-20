package info.openmeta.framework.orm.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * ID generation strategy, controlled by model metadata:
 *      DB_AUTO_ID: database auto-increment ID, as default,
 *      SHORT_UUID: program-generated short UUID (Base 62, up to 22 characters),
 *      UUID: standard UUID without hyphens,
 *      EXTERNAL_ID: external input ID.
 */
@Getter
@AllArgsConstructor
public enum IdStrategy {
    DB_AUTO_ID("DbAutoId", "Database auto-increment ID"),
    SHORT_UUID("ShortUUID", "Short UUID"),
    UUID("UUID", "UUID"),
    EXTERNAL_ID("ExternalId", "External ID");

    @JsonValue
    private final String type;

    private final String name;
}
