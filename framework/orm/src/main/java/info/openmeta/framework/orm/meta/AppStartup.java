package info.openmeta.framework.orm.meta;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.jdbc.CannotGetJdbcConnectionException;
import org.springframework.stereotype.Component;

/**
 * Load the global system cache, using the highest priority Runner.
 */
@Component
@Order(Ordered.HIGHEST_PRECEDENCE)
public class AppStartup implements InitializingBean {

    @Autowired
    ModelManager modelManager;

    @Autowired
    OptionManager optionManager;

    @Override
    public void afterPropertiesSet() {
        try {
            // 1. init model manager
            modelManager.init();
            // 2. init option manager
            optionManager.init();
        } catch (CannotGetJdbcConnectionException e) {
            throw new RuntimeException("Database connection failed, please check the database configuration.");
        }
    }
}
