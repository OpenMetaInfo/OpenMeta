package info.openmeta.starter.file.config;

import com.aliyun.oss.OSS;
import com.aliyun.oss.OSSClientBuilder;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class OssConfig {

    @Value("${aliyun.accessKeyId:}")
    private String ossAccessKey;

    @Value("${aliyun.accessKeySecret:}")
    private String ossSecretKey;

    @Value("${aliyun.endpoint:}")
    private String host;

    @Value("${aliyun.bucketName:}")
    private String bucketName;

    @Bean
    public OSS ossClient() {
        return new OSSClientBuilder().build(host, ossAccessKey, ossSecretKey);
    }
}
