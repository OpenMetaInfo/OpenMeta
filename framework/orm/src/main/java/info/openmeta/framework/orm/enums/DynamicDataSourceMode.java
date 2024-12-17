package info.openmeta.framework.orm.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum DynamicDataSourceMode {
    /**
     * read-write-separation mode.
     */
    READ_WRITE_SEPARATION("read-write-separation"),
    /**
     * multi-tenancy-isolated mode.
     */
    MULTI_TENANCY_ISOLATED("multi-tenancy-isolated"),
    /**
     * switch-by-model mode.
     */
    SWITCH_BY_MODEL("switch-by-model"),
    /**
     * multi-datasource mode, which is the default mode.
     */
    MULTI_DATASOURCE("multi-datasource"),
    ;

    @JsonValue
    private final String mode;

}
