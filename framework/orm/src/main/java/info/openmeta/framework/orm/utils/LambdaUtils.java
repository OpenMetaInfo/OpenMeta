package info.openmeta.framework.orm.utils;

import info.openmeta.framework.base.utils.SFunction;

import java.beans.Introspector;
import java.lang.invoke.SerializedLambda;
import java.lang.reflect.Method;

/**
 * Lambda expression utility class
 */
public class LambdaUtils {

    private LambdaUtils() {}

    /**
     * Get the attribute name corresponding to the lambda expression,
     * such as extracting the string 'name' from UserProfile::getName().
     *
     * @param lambdaMethod lambda expression
     * @return attribute name string
     */
    public static <T, R> String getAttributeName(SFunction<T, R> lambdaMethod) {
        try {
            Method method = lambdaMethod.getClass().getDeclaredMethod("writeReplace");
            method.setAccessible(true);
            SerializedLambda serializedLambda = (SerializedLambda) method.invoke(lambdaMethod);
            return methodToAttribute(serializedLambda.getImplMethodName());
        } catch (Exception e) {
            throw new IllegalArgumentException(e);
        }
    }

    /**
     * Extract attribute name from method name: 'getName' -> 'name'
     *
     * @param methodName method name
     * @return attribute name
     */
    private static String methodToAttribute(String methodName) {
        if (methodName.startsWith("get") || methodName.startsWith("set")) {
            return Introspector.decapitalize(methodName.substring(3));
        } else if (methodName.startsWith("is")) {
            return Introspector.decapitalize(methodName.substring(2));
        } else {
            throw new IllegalArgumentException("Method name must start with 'get' or 'is'");
        }
    }


}
