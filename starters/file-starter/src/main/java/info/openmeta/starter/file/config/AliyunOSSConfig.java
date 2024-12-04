package info.openmeta.starter.file.config;

import com.aliyun.oss.OSS;
import com.aliyun.oss.OSSClientBuilder;
import info.openmeta.starter.file.oss.OSSProperties;
import info.openmeta.starter.file.oss.OssClientService;
import info.openmeta.starter.file.oss.impl.AliyunOSSClientService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConditionalOnProperty(value = "oss.type", havingValue = "aliyun")
public class AliyunOSSConfig {

    @Autowired
    private OSSProperties ossProperties;

    @Bean
    public OSS oss() {
        return new OSSClientBuilder()
                .build(ossProperties.getEndpoint(), ossProperties.getAccessKey(), ossProperties.getSecretKey());
    }

    @Bean
    @ConditionalOnBean({OSS.class})
    @ConditionalOnMissingBean(OssClientService.class)
    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    public AliyunOSSClientService aliyunOSSClientService(OSS oss) {
        return new AliyunOSSClientService(oss, ossProperties);
    }
}
