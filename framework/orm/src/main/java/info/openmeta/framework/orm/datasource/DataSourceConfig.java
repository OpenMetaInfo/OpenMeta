package info.openmeta.framework.orm.datasource;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.enums.DatabaseType;
import info.openmeta.framework.orm.enums.DynamicDataSourceMode;
import info.openmeta.framework.orm.jdbc.database.DBUtil;
import info.openmeta.framework.orm.jdbc.database.dialect.DialectInterface;
import info.openmeta.framework.orm.meta.ModelManager;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.jdbc.DataSourceBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.transaction.PlatformTransactionManager;

import javax.sql.DataSource;
import java.util.HashMap;
import java.util.Map;

/**
 * Data source configuration
 * Load data source configuration from application.yml
 * Example:
 * <p>
 * spring:
 *   datasource:
 *     dynamic:
 *       enable: true
 *       mode: read-write-separation
 *       datasource:
 *         primary:
 *           driver-class-name: com.mysql.cj.jdbc.Driver
 *           url: jdbc:mysql://localhost:3306/primary?useUnicode=true&characterEncoding=utf-8&useSSL=false
 *           username: root
 *           password: root
 *         ds1:
 *           driver-class-name: com.mysql.cj.jdbc.Driver
 *           url: jdbc:mysql://localhost:3306/ds1?useUnicode=true&characterEncoding=utf-8&useSSL=false
 *           username: root
 *           password: root
 *         ds2:
 *           driver-class-name: com.mysql.cj.jdbc.Driver
 *           url: jdbc:mysql://localhost:3306/ds2?useUnicode=true&characterEncoding=utf-8&useSSL=false
 *           username: root
 *           password: root
 * </p>
 */
@Slf4j
@Configuration
@ConditionalOnProperty(name = "spring.datasource.dynamic.enable", havingValue = "true")
public class DataSourceConfig {

    /** Store the dialect of each data source */
    private static final Map<String, DialectInterface> dataSourceDialectMap = new HashMap<>();

    /** Dynamic data source mode, default is multi-datasource */
    @Value("${spring.datasource.dynamic.mode:multi-datasource}")
    private DynamicDataSourceMode dynamicDataSourceMode;

    @Autowired
    private DynamicDataSourceProperties dynamicDataSourceProperties;

    /** Default datasource key, that is the first datasource */
    private static String defaultDataSourceKey;

    @Bean
    @Primary
    public DataSource dataSource() {
        Map<Object, Object> targetDataSources = new HashMap<>();
        DataSource defaultDataSource = null;
        // Get datasource configuration
        Map<String, DataSourceProperties> datasourceMap = dynamicDataSourceProperties.getDatasource();
        if (datasourceMap == null || datasourceMap.isEmpty()) {
            throw new RuntimeException("Multiple-datasource configuration is empty!");
        }
        Map<DatabaseType, DialectInterface> dbTypeDialectMap = new HashMap<>();
        for (Map.Entry<String, DataSourceProperties> entry : datasourceMap.entrySet()) {
            String dsKey = entry.getKey();
            DataSourceProperties dsProps = entry.getValue();
            // Build dataSource
            DataSourceBuilder<?> factory = DataSourceBuilder.create()
                    .driverClassName(dsProps.getDriverClassName())
                    .url(dsProps.getUrl())
                    .username(dsProps.getUsername())
                    .password(dsProps.getPassword());
            DataSource dataSource = factory.build();
            // Put dataSource to targetDataSources
            targetDataSources.put(dsKey, dataSource);
            // Extract databaseType from url, and create dialect
            DatabaseType databaseType = DBUtil.parseDatabaseType(dsProps.getUrl());
            DialectInterface dialect = dbTypeDialectMap.computeIfAbsent(databaseType, DBUtil::createDialect);
            dataSourceDialectMap.put(dsKey, dialect);

            // If defaultDataSource is not set, set it to the first dataSource
            if (defaultDataSource == null) {
                defaultDataSource = dataSource;
                defaultDataSourceKey = dsKey;
            } else if (DynamicDataSourceMode.READ_WRITE_SEPARATION.equals(dynamicDataSourceMode)) {
                // If read-write-separation is configured, the first dataSource is the primary dataSource
                // The other dataSource is the readonly dataSource
                ReadonlyDataSourceHolder.addReadonlyDataSourceKey(dsKey);
            }
        }
        // Create dynamicDataSource
        DynamicDataSource dynamicDataSource = new DynamicDataSource();
        dynamicDataSource.setTargetDataSources(targetDataSources);
        dynamicDataSource.setDefaultTargetDataSource(defaultDataSource);
        log.info("Multi-datasource initialized successfully!");
        return dynamicDataSource;
    }

    /**
     * Get the current database dialect in multi-datasource scenario
     *
     * @return DialectInterface
     */
    public static DialectInterface getCurrentDialect() {
        String dsKey = getCurrentDataSourceKey();
        Assert.isTrue(dataSourceDialectMap.containsKey(dsKey),
                "The datasource key `{0}` is not exist in multi-datasource configuration!", dsKey);
        return dataSourceDialectMap.get(getCurrentDataSourceKey());
    }

    /**
     * Get the current datasource key
     * @return datasource key
     */
    public static String getCurrentDataSourceKey() {
        String currentDataSourceKey = DataSourceContextHolder.getDataSourceKey();
        if (StringUtils.isBlank(currentDataSourceKey)) {
            currentDataSourceKey = defaultDataSourceKey;
        }
        return currentDataSourceKey;
    }

    /**
     * Get the primary datasource key, that is the first datasource
     * @return datasource key
     */
    public static String getPrimaryDataSourceKey() {
        return defaultDataSourceKey;
    }

    /**
     * Get the datasource key by model name
     * @param modelName model name
     * @return datasource key
     */
    public static String getDataSourceKeyByModel(String modelName) {
        return ModelManager.getModel(modelName).getDataSource();
    }

    /**
     * Transaction manager for dynamic datasource
     *
     * @param dataSource DataSource
     * @return PlatformTransactionManager
     */
    @Bean
    public PlatformTransactionManager transactionManager(DataSource dataSource) {
        return new DataSourceTransactionManager(dataSource);
    }
}
