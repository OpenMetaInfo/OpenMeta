package info.openmeta.framework.orm.aspect;

import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.orm.annotation.SwitchUser;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;


@Aspect
@Component
@Slf4j
public class SwitchUserAspect {

    /**
     * Switch current user to the specified system level user, in order to access the system resources.
     */
    @Around("@annotation(switchUser)")
    public Object around(ProceedingJoinPoint joinPoint, SwitchUser switchUser) throws Throwable {
        Context originalContext = ContextHolder.getContext();
        try {
            Context clonedContext = ContextHolder.cloneContext();
            clonedContext.setName(switchUser.value().getName());
            // Skip permission check for system level users.
            clonedContext.setSkipPermissionCheck(true);
            // Switch context
            ContextHolder.setContext(clonedContext);
            return joinPoint.proceed();
        } finally {
            // Recovery context
            ContextHolder.setContext(originalContext);
        }
    }

}
