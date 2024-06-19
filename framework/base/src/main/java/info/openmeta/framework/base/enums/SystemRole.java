package info.openmeta.framework.base.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Internal System Role
 */
@Getter
@AllArgsConstructor
public enum SystemRole {
    SYSTEM_ROLE_ADMIN("SystemRoleAdmin", "System Admin Role"),
    SYSTEM_ROLE_TEST("SystemRoleTest", "System Test Role"),;

    @JsonValue
    private final String code;
    private final String name;
}
