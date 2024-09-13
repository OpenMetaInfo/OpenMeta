package info.openmeta.starter.file.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Import Rule Enum
 */
@Getter
@AllArgsConstructor
public enum ImportRule {
    CREATE_OR_UPDATE("CreateOrUpdate", "Create or Update"),
    ONLY_UPDATE("OnlyUpdate", "Only Update"),
    ONLY_CREATE("OnlyCreate", "Only Create");

    @JsonValue
    private final String code;
    private final String name;
}
