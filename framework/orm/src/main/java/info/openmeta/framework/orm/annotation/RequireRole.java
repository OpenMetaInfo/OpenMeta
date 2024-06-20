package info.openmeta.framework.orm.annotation;

import info.openmeta.framework.base.enums.SystemRole;

import java.lang.annotation.*;

/**
 * Annotation to specify the system role required to access the method.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface RequireRole {
    SystemRole value();
}