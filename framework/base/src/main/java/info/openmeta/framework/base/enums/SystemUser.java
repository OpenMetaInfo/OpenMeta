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
    SUPER_ADMIN("SuperAdmin", "Super Admin User"),
    CRON_USER("CronUser", "Cron Job User"),
    BOOT_USER("BootUser", "System Boot User"),
    INTEGRATION_USER("IntegrationUser", "Integration User"),
    ANONYMOUS_USER("AnonymousUser", "Anonymous User"),
    TEST_USER("TestUser", "Test User"),;

    @JsonValue
    private final String code;
    private final String name;
}
