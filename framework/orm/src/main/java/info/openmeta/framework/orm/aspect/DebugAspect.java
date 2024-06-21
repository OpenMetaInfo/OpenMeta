package info.openmeta.framework.orm.aspect;

import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.utils.Assert;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;


/**
 * Debug annotation aspect.
 */
@Aspect
@Component
public class DebugAspect {

    @Around("@annotation(info.openmeta.framework.orm.annotation.Debug)")
    public Object around(ProceedingJoinPoint joinPoint) throws Throwable {
        Context context = ContextHolder.getContext();
        Assert.notNull(context, "Context is null!");
        boolean previousValue = context.isDebug();
        try {
            context.setDebug(true);
            return joinPoint.proceed();
        } finally {
            context.setDebug(previousValue);
        }
    }

}
