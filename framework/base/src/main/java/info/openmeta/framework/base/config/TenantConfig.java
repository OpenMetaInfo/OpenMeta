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

    @Getter
    private static boolean enableMultiTenancy;

    @PostConstruct
    public void init() {
        enableMultiTenancy = multiTenancy;
    }

}
