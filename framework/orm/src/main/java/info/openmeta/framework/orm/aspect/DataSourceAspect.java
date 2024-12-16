package info.openmeta.framework.orm.aspect;

import info.openmeta.framework.orm.annotation.DataSource;
import info.openmeta.framework.orm.datasource.DataSourceContextHolder;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;


/**
 * Datasource annotation aspect.
 * Switch the datasource according to the annotation.
 * If the method does not have the DataSource annotation, get the class level annotation.
 * If the previous data source is the same as the current data source, no need to switch.
 * If the previous data source is different from the current data source, throw an exception.
 * If the previous data source is null, set the current data source.
 * If the data source needs to be switched, clear the data source after the method is executed.
 */
@Aspect
@Component
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
               throw new RuntimeException("""
                       Cannot switch to different data source in the same transaction.
                       If you're doing a non-transactional operation, you need to clear datasource manually
                        by executing `DataSourceContextHolder.clearDataSourceKey()`
                       """);
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
