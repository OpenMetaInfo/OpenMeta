package info.openmeta.framework.orm.aspect;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.Cast;
import info.openmeta.framework.orm.datasource.DataSourceConfig;
import info.openmeta.framework.orm.jdbc.database.SqlParams;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StopWatch;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * Print the SQL log aspect according to debug mode.
 */
@Slf4j
@Aspect
@Component
public class LogSqlAspect {

    private static final int LOG_BATCH_NUMBER = 3;

    @Value("${spring.datasource.dynamic.enable:false}")
    private Boolean enableMultiDataSource;

    @Around("@annotation(info.openmeta.framework.orm.annotation.LogSql)")
    public Object around(ProceedingJoinPoint joinPoint) throws Throwable {
        if (ContextHolder.getContext().isDebug()) {
            // Get method arguments
            Object[] methodArgs = joinPoint.getArgs();
            StringBuilder sb = logSql(methodArgs);
            StopWatch stopWatch = new StopWatch();
            stopWatch.start();
            try {
                Object result = joinPoint.proceed();
                appendResult(sb, result);
                return result;
            } finally {
                stopWatch.stop();
                sb.append("\nTime: ").append(stopWatch.getTotalTimeMillis()).append(" ms");
                // Log SQL even if an exception occurs, no execution result when an exception occurs
                log.warn(sb.toString());
            }
        }
        return joinPoint.proceed();
    }

    /**
     * Print SQL log in debug mode, including SQL, and parameter values.
     * @param methodArgs Method arguments
     * @return SQL log string
     */
    private StringBuilder logSql(Object[] methodArgs) {
        Assert.isTrue(methodArgs.length > 0, "Missing JDBC parameters!");
        StringBuilder sb = new StringBuilder("\n");
        if (Boolean.TRUE.equals(enableMultiDataSource)) {
            sb.append(DataSourceConfig.getCurrentDataSourceKey()).append(" in multi-datasource. ");
        }
        if (methodArgs[0] instanceof SqlParams sqlParams) {
            sb.append("SQL: \n").append(sqlParams.toLogString());
            if (methodArgs.length > 1 && methodArgs[1] instanceof List && !((List<?>) methodArgs[1]).isEmpty()) {
                appendBatchParams(sb, methodArgs);
            }
        }
        return sb;
    }

    /**
     * Append SQL parameter values.
     * If the number of rows exceeds 3 rows, only the first three rows are printed.
     * @param sb SQL string object
     * @param methodArgs Method arguments
     */
    private void appendBatchParams(StringBuilder sb, Object[] methodArgs) {
        List<Object[]> batchArgs = Cast.of(methodArgs[1]);
        sb.append("\nBatch Params: \n");
        if (batchArgs.size() > LOG_BATCH_NUMBER) {
            batchArgs.subList(0, LOG_BATCH_NUMBER).forEach(row -> sb.append(Arrays.toString(row)).append("\n"));
            sb.append(" ... Total rows: ").append(batchArgs.size());
        } else {
            batchArgs.forEach(row -> sb.append(Arrays.toString(row)).append("\n"));
        }
    }

    /**
     * Append result log.
     * If the result is a list and exceeds 3 rows, only the first three rows are printed.
     * @param sb SQL string object
     * @param result Result object
     */
    private void appendResult(StringBuilder sb, Object result) {
        sb.append("\nResults: ");
        if (result instanceof List && ((List<?>) result).stream().anyMatch(Map.class::isInstance)) {
            List<Map<String, Object>> resultList = Cast.of(result);
            sb.append(resultList.size()).append(" rows\n");
            if (resultList.size() > LOG_BATCH_NUMBER) {
                resultList.subList(0, LOG_BATCH_NUMBER).forEach(row -> sb.append(row.values()).append("\n"));
                sb.append(" ... ");
            } else {
                resultList.forEach(row -> sb.append(row.values()).append("\n"));
            }
        } else if (result instanceof Map) {
            sb.append("\n").append(((Map<?, ?>) result).values());
        } else {
            sb.append(result);
        }
    }
}
