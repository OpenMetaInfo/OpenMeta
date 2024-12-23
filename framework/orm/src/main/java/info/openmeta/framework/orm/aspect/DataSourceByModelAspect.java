package info.openmeta.framework.orm.aspect;

import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.datasource.DataSourceConfig;
import info.openmeta.framework.orm.datasource.DataSourceContextHolder;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionSynchronizationManager;


/**
 * Datasource routing according to the model data source key.
 * 1. The first parameter of the annotated method must be the model name.
 * 2. If the model-level datasource is not configured,
 *    or the model-level datasource is the same as the current datasource, no need to switch.
 * 3. If in transaction, cannot switch datasource, throw an exception.
 * 4. If the model-level datasource is different from the current datasource, switch datasource.
 * 5. After the method is executed, recover the previous datasource.
 * 6. The system model cannot switch datasource, which uses the primary datasource.
 */
@Aspect
@Component
@Order(Ordered.HIGHEST_PRECEDENCE)
@ConditionalOnProperty(name = "spring.datasource.dynamic.enable", havingValue = "true")
@ConditionalOnExpression("'${spring.datasource.dynamic.mode:}' == 'switch-by-model'")
public class DataSourceByModelAspect {

    /**
     * The first parameter of the annotated method must be the model name.
     */
    @Around("@annotation(info.openmeta.framework.orm.annotation.ExecuteSql)")
    public Object around(ProceedingJoinPoint joinPoint) throws Throwable {
        String previousDataSource = DataSourceContextHolder.getDataSourceKey();
        Object[] methodArgs = joinPoint.getArgs();
        String modelName = (String) methodArgs[0];
        // The system model cannot switch datasource.
        if (ModelConstant.SYSTEM_MODEL.contains(modelName)) {
            return joinPoint.proceed();
        }
        String modelDataSourceKey = DataSourceConfig.getDataSourceKeyByModel(modelName);
        // If the model-level datasource is not configured,
        // or the model-level datasource is the same as the current datasource, no need to switch.
        if (StringUtils.isBlank(modelDataSourceKey) || modelDataSourceKey.equals(previousDataSource)) {
            return joinPoint.proceed();
        }
        try {
            boolean isTransactionActive = TransactionSynchronizationManager.isActualTransactionActive();
            if (isTransactionActive) {
                // In transaction, cannot switch datasource, throw an exception.
                throw new RuntimeException("""
                        Cannot switch to different data source in the same transaction.
                        If you're doing a non-transactional operation, you need to clear datasource manually
                        by executing `DataSourceContextHolder.clearDataSourceKey()`
                        """);
            } else {
                DataSourceContextHolder.setDataSourceKey(modelDataSourceKey);
            }
            return joinPoint.proceed();
        } finally {
            DataSourceContextHolder.setDataSourceKey(previousDataSource);
        }
    }

}
