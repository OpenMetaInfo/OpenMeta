package info.openmeta.framework.base.config;

import jakarta.annotation.PostConstruct;
import lombok.Getter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

/**
 * Multi-tenancy configuration
 */
@Configuration
public class TenantConfig {

    @Value("${system.multi-tenancy.enable:false}")
    private boolean multiTenancy;

    @Value("${system.multi-tenancy.isolated-database:false}")
    private boolean isolatedDatabase;

    @Value("${spring.datasource.dynamic.read-write-separation:false}")
    private boolean readWriteSeparation;

    @Getter
    private static boolean enableMultiTenancy;

    @PostConstruct
    public void init() {
        if (isolatedDatabase & readWriteSeparation) {
            throw new RuntimeException(
                    "The isolated-database multi-tenancy and read-write-separation cannot be enabled at the same time!");
        }
        enableMultiTenancy = multiTenancy;
    }

}
