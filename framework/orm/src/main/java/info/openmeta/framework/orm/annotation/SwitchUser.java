package info.openmeta.framework.orm.annotation;

import info.openmeta.framework.base.enums.SystemUser;

import java.lang.annotation.*;

/**
 * Switch current user to the specified system level user, in order to access the system resources.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface SwitchUser {

    SystemUser value() default SystemUser.SUPER_ADMIN;
}