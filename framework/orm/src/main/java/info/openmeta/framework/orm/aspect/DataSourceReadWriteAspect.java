package info.openmeta.framework.orm.aspect;

import info.openmeta.framework.orm.annotation.WriteOperation;
import info.openmeta.framework.orm.datasource.DataSourceConfig;
import info.openmeta.framework.orm.datasource.DataSourceContextHolder;
import info.openmeta.framework.orm.datasource.ReadonlyDataSourceHolder;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.lang.reflect.Method;


/**
 * Read-write-separation datasource aspect.
 * DataSource routing according to the read-write-separation strategy.
 *  1. If in transaction, use the primary datasource.
 *  2. If not in transaction, and execute write operation, use the primary datasource.
 *  3. If not in transaction and not execute write operation, and already set datasource, use the specified datasource.
 *  4. If not in transaction and not execute write operation, and not set datasource, use the random readonly datasource.
 */
@Aspect
@Component
@Order(Ordered.HIGHEST_PRECEDENCE)
@ConditionalOnProperty(name = "spring.datasource.dynamic.enable", havingValue = "true")
@ConditionalOnExpression("'${spring.datasource.dynamic.mode:}' == 'read-write-separation'")
public class DataSourceReadWriteAspect {

    @Around("@annotation(info.openmeta.framework.orm.annotation.ExecuteSql)")
    public Object around(ProceedingJoinPoint joinPoint) throws Throwable {
        String previousDataSource = DataSourceContextHolder.getDataSourceKey();
        try {
            boolean isTransactionActive = TransactionSynchronizationManager.isActualTransactionActive();
            String primaryDataSourceKey = DataSourceConfig.getPrimaryDataSourceKey();
            if (isTransactionActive) {
                // In transaction, use the primary datasource.
                DataSourceContextHolder.setDataSourceKey(primaryDataSourceKey);
            } else {
                MethodSignature signature = (MethodSignature) joinPoint.getSignature();
                Method method = signature.getMethod();
                WriteOperation writeOperation = method.getAnnotation(WriteOperation.class);
                if (writeOperation != null) {
                    // Not in transaction, but execute write operation, use the primary datasource.
                    DataSourceContextHolder.setDataSourceKey(primaryDataSourceKey);
                } else if (StringUtils.isBlank(previousDataSource)) {
                    // Not in transaction, not execute write operation, and not set datasource, use the readonly datasource.
                    String readonlyDataSourceKey = ReadonlyDataSourceHolder.getReadonlyDataSourceKey();
                    DataSourceContextHolder.setDataSourceKey(readonlyDataSourceKey);
                }
                // Not in transaction, but already set datasource, use the specified datasource.
            }
            return joinPoint.proceed();
        } finally {
            DataSourceContextHolder.setDataSourceKey(previousDataSource);
        }
    }

}
