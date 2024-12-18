package info.openmeta.framework.orm.aspect;

import info.openmeta.framework.orm.annotation.DataSource;
import info.openmeta.framework.orm.datasource.DataSourceContextHolder;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.lang.reflect.Method;


/**
 * Datasource annotation aspect.
 * Switch the datasource according to the annotation.
 *  1. If the method does not have the DataSource annotation, get the class level annotation.
 *  2. If the previous data source is the same as the current data source, no need to switch.
 *  3. If the previous data source is different from the current data source, throw an exception.
 *  4. If the previous data source is null, set the current data source.
 *  5. If the data source needs to be switched, clear the data source after the method is executed.
 */
@Aspect
@Component
@ConditionalOnProperty(name = "spring.datasource.dynamic.enable", havingValue = "true")
public class DataSourceAspect {

    @Around("@annotation(info.openmeta.framework.orm.annotation.DataSource)" +
            " || @within(info.openmeta.framework.orm.annotation.DataSource)")
    public Object around(ProceedingJoinPoint joinPoint) throws Throwable {
        MethodSignature signature = (MethodSignature) joinPoint.getSignature();
        Method method = signature.getMethod();
        DataSource dataSource = method.getAnnotation(DataSource.class);
        if (dataSource == null) {
            // If the method does not have the DataSource annotation, get the class level annotation.
            dataSource = joinPoint.getTarget().getClass().getAnnotation(DataSource.class);
        }
        // Check if the data source needs to be switched.
        String previousDataSource = DataSourceContextHolder.getDataSourceKey();
        if (dataSource != null) {
            String currentDataSource = dataSource.value();
            if (previousDataSource == null) {
                DataSourceContextHolder.setDataSourceKey(currentDataSource);
            } else if (!previousDataSource.equals(currentDataSource)) {
                if (TransactionSynchronizationManager.isActualTransactionActive()) {
                    // If in transaction, throw an exception.
                    throw new RuntimeException("""
                            Cannot switch to different data source in the same transaction.
                            If you're doing a non-transactional operation, you need to clear datasource manually
                            by executing `DataSourceContextHolder.clearDataSourceKey()`
                            """);
                }
            }
            // If the previous data source is the same as the current data source, no need to switch.
        }
        // Proceed the method.
        try {
            return joinPoint.proceed();
        } finally {
            DataSourceContextHolder.setDataSourceKey(previousDataSource);
        }
    }

}
