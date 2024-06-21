package info.openmeta.framework.orm.aspect;

import info.openmeta.framework.orm.annotation.DataMask;
import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;


/**
 * DataMask annotation aspect.
 */
@Aspect
@Component
public class DataMaskAspect {

    /**
     * Around aspect with dataMask annotation, the default value of dataMask is true
     * @param joinPoint Around join point object
     * @param dataMask DataMask annotation object
     * @return Original method return value
     * @throws Throwable Exception
     */
    @Around("@annotation(dataMask)")
    public Object around(ProceedingJoinPoint joinPoint, DataMask dataMask) throws Throwable {
        Context context = ContextHolder.getContext();
        boolean previousValue = context.isDataMask();
        try {
            context.setDataMask(dataMask.value());
            return joinPoint.proceed();
        } finally {
            context.setDataMask(previousValue);
        }
    }

}
