package info.openmeta.framework.base.utils;

import java.io.Serializable;
import java.util.function.Function;

/**
 * Serializable function, used to extract property names based on lambda functions
 * For example, based on UserProfile::getCompanyId(), extract the fieldName 'companyId'
 * @param <T> Entity type
 * @param <R> Return type
 */
@FunctionalInterface
public interface SFunction<T, R> extends Function<T, R>, Serializable{

}