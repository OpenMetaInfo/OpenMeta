package info.openmeta.framework.orm.aspect;

import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

/**
 * Read primary annotation aspect.
 * Set the readPrimary attribute of Context to pass the hint of reading the primary database.
 */
@Aspect
@Component
public class ReadPrimaryAspect {

    @Around("@annotation(info.openmeta.framework.orm.annotation.ReadPrimary)")
    public Object around(ProceedingJoinPoint joinPoint) throws Throwable {
        Context context = ContextHolder.getContext();
        boolean isReadPrimary = context.isReadPrimary();
        try {
            context.setReadPrimary(true);
            return joinPoint.proceed();
        } finally {
            context.setReadPrimary(isReadPrimary);
        }
    }

}
