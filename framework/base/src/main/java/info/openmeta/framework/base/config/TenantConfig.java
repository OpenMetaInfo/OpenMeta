package info.openmeta.framework.base.config;

import jakarta.annotation.PostConstruct;
import lombok.Getter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

/**
 * Multi tenant configuration
 */
@Configuration
public class TenantConfig {

    @Value("${enable.multiTenant:false}")
    private static boolean multiTenant;

    @Getter
    private static boolean enableMultiTenant;

    @PostConstruct
    public void init() {
        enableMultiTenant = multiTenant;
    }

}
