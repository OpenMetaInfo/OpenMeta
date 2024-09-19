package info.openmeta.framework.base.utils;

import lombok.Getter;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;

import java.beans.Introspector;
import java.lang.annotation.Annotation;
import java.util.Map;

/**
 * Spring Context Util
 */
@Component
public class SpringContextUtils implements ApplicationContextAware {

    @Getter
    private static ApplicationContext applicationContext;

    @SuppressWarnings("NullableProblems")
    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        synchronized (SpringContextUtils.class) {
            if (SpringContextUtils.applicationContext == null) {
                SpringContextUtils.applicationContext = applicationContext;
            }
        }
    }

    public static boolean existApplicationContext() {
        return applicationContext != null;
    }

    /**
     * Get the bean by name
     */
    @SuppressWarnings("unchecked")
    public static <T> T getBeanByName(String name) {
        checkApplicationContext();
        name = convertToBeanName(name);
        return (T) getApplicationContext().getBean(name);
    }

    /**
     * Get the bean by class
     */
    public static <T> T getBeanByClass(Class<T> clazz) {
        checkApplicationContext();
        return getApplicationContext().getBean(clazz);
    }

    /**
     * Get the bean map by annotation
     */
    public static Map<String, Object> getBeanWithAnnotation(Class<? extends Annotation> annotationType) {
        checkApplicationContext();
        return getApplicationContext().getBeansWithAnnotation(annotationType);
    }

    /**
     * Get the bean by name and class
     */
    public static <T> T getBean(String name, Class<T> clazz) {
        checkApplicationContext();
        name = convertToBeanName(name);
        return getApplicationContext().getBean(name, clazz);
    }

    /**
     * Check if the applicationContext is injected
     */
    private static void checkApplicationContext() {
        if (applicationContext == null) {
            throw new ExceptionInInitializerError("applicationContext is not injected.");
        }
    }

    /**
     * Convert the class name to the bean name according to the Spring naming convention
     * Examples:
     *      MyService -> myService: Convert the first letter to lowercase when the second letter is lowercase.
     *      URLParser -> URLParser: Return the original string if the first two letters are uppercase.
     *
     * @param name The class name or bean name
     * @return The converted bean name of Spring
     */
    public static String convertToBeanName(String name) {
        return Introspector.decapitalize(name);
    }
}


