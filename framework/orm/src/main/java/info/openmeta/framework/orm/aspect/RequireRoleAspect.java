package info.openmeta.framework.orm.aspect;

import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.utils.Assert;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

/**
 * RequireRole annotation aspect.
 * Check if the current user has the specified role permission.
 */
@Aspect
@Component
public class RequireRoleAspect {

    @Around("@annotation(info.openmeta.framework.orm.annotation.RequireRole)")
    public Object around(ProceedingJoinPoint joinPoint) throws Throwable {
        Context context = ContextHolder.getContext();
        Assert.notNull(context, "Context is null!");
        // TODO: User role check, if the user does not have the specified role, throw an exception.
        // Skip permission check after role check.
        boolean previousIgnoreValue = context.isSkipPermissionCheck();
        try {
            context.setSkipPermissionCheck(true);
            return joinPoint.proceed();
        } finally {
            context.setSkipPermissionCheck(previousIgnoreValue);
        }
    }

}
