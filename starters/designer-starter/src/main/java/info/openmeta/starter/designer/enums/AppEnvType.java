package info.openmeta.starter.designer.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum AppEnvType {
    DEV("Dev", "Dev"),
    TEST("Test", "Test"),
    UAT("UAT", "UAT"),
    PROD("Prod", "Prod");

    @JsonValue
    private final String code;
    private final String name;
}
