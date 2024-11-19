package info.openmeta.framework.orm.annotation;

import java.lang.annotation.*;

/**
 * Datasource annotation, to switch the datasource.
 */
@Target({ ElementType.METHOD, ElementType.TYPE })
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface DataSource {
    String value();
}