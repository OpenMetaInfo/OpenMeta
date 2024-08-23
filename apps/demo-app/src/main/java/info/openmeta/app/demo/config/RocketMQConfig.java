package info.openmeta.app.demo.config;

import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class RocketMQConfig {

    @Bean
    public RocketMQTemplate rocketMQTemplate() {
        return new RocketMQTemplate();
    }
}
