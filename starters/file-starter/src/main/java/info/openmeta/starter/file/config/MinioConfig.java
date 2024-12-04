package info.openmeta.starter.file.config;

import info.openmeta.starter.file.oss.OSSProperties;
import info.openmeta.starter.file.oss.OssClientService;
import info.openmeta.starter.file.oss.impl.MinioClientService;
import io.minio.MinioClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConditionalOnProperty(value = "oss.type", havingValue = "minio")
public class MinioConfig {

    @Autowired
    private OSSProperties ossProperties;

    @Bean(name = "minioClient")
    public MinioClient minioClient() {
        return MinioClient.builder()
                .endpoint(ossProperties.getEndpoint())
                .credentials(ossProperties.getAccessKey(), ossProperties.getSecretKey())
                .build();
    }


    @Bean
    @ConditionalOnBean(MinioClient.class)
    @ConditionalOnMissingBean(OssClientService.class)
    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    public MinioClientService minioClientService(MinioClient minioClient) {
        return new MinioClientService(minioClient, ossProperties);
    }
}
