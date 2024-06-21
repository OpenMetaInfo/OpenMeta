package info.openmeta.framework.orm.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This annotation usually used in the Controller layer.
 * Masking sensitive fields with ****, currently only supports `String` type fields.
 * If a field value is needed in the internal method, add @DataMask(false) annotation to the method.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface DataMask {
    boolean value() default true;
}