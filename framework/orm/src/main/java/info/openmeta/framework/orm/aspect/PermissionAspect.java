package info.openmeta.framework.orm.aspect;

import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

/**
 * Aspect for permission check.
 */
@Aspect
@Component
public class PermissionAspect {

    /**
     * Around aspect with SkipPermissionCheck annotation.
     * Do not check permission from the annotated method, but the context user still keeps the current user.
     * @param joinPoint Around join point object
     * @return Original method return value
     * @throws Throwable Exception
     */
    @Around("@annotation(info.openmeta.framework.orm.annotation.SkipPermissionCheck)")
    public Object around(ProceedingJoinPoint joinPoint) throws Throwable {
        Context context = ContextHolder.getContext();
        boolean previousValue = context.isSkipPermissionCheck();
        try {
            context.setSkipPermissionCheck(true);
            return joinPoint.proceed();
        } finally {
            context.setSkipPermissionCheck(previousValue);
        }
    }

}
