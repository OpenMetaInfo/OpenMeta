package info.openmeta.starter.file.oss;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

@Data
@Component
@ConfigurationProperties(prefix = "oss")
@Validated
public class OSSProperties {

    @NotBlank(message = "The type of OSS must be specified. The value can be 'minio' or 'aliyun'.")
    private String type;

    @NotBlank(message = "The endpoint of OSS must be specified.")
    private String endpoint;

    @NotBlank(message = "The access key of OSS must be specified.")
    private String accessKey;

    @NotBlank(message = "The secret key of OSS must be specified.")
    private String secretKey;

    @NotBlank(message = "The bucket name of OSS must be specified.")
    private String bucketName;

    private Integer urlExpireSeconds;
}
