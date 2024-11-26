package info.openmeta.framework.orm.datasource;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.LinkedHashMap;
import java.util.Map;

@Getter
@Setter
@Configuration
@ConfigurationProperties(prefix = "spring.datasource.dynamic")
public class DynamicDataSourceProperties {

    private Boolean enable;
    private Boolean readWriteSeparation;

    private Map<String, DataSourceProperties> datasource = new LinkedHashMap<>();

}