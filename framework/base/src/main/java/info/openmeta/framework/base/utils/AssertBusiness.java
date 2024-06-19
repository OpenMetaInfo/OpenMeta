package info.openmeta.framework.base.utils;

import info.openmeta.framework.base.exception.BusinessException;

public abstract class AssertBusiness extends Assert {

    /** Throw BusinessException. */
    private static void throwException(String message, Object... args) {
        throw new BusinessException(message, args);
    }
}
