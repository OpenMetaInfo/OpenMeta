package info.openmeta.framework.web.interceptor;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.context.UserInfo;
import jakarta.annotation.Nonnull;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;

/**
 * Context interceptor interface.
 * Implementations are responsible for extracting the current user context from the request,
 * including details such as user identity and preferences.
 */
@Component
public interface ContextInterceptor extends HandlerInterceptor {

    /**
     * Setup user context with user info.
     * Extract the `debug` parameter from the URI to enable debug mode.
     *
     * @param request the current HTTP request
     * @param userInfo the user info
     */
    void setupUserContext(HttpServletRequest request, UserInfo userInfo);

    /**
     * Clean up the threadLocal after request completion.
     *
     * @param request  the current HTTP request
     * @param response the HTTP response
     * @param handler  the object that handles the HTTP request
     * @param e        exception
     */
    @Override
    default void afterCompletion(@Nonnull HttpServletRequest request,
                                        @Nonnull HttpServletResponse response,
                                        @Nonnull Object handler,
                                        Exception e) {
        ContextHolder.removeContext();
    }

}
