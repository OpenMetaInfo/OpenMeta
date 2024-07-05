package info.openmeta.framework.base.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Internal System User
 * AnonymousUser: Operation before ready for API request
 */
@Getter
@AllArgsConstructor
public enum SystemUser {
    SUPER_ADMIN("SuperAdmin", "Super Admin"),
    SYSTEM_USER("SystemUser", "System"),
    CRON_USER("CronUser", "Schedule Job"),
    INTEGRATION_USER("IntegrationUser", "Integration"),
    ANONYMOUS_USER("AnonymousUser", "Anonymous"),
    TEST_USER("TestUser", "Test"),;

    @JsonValue
    private final String code;
    private final String name;
}
