package info.openmeta.framework.web.interceptor;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;

/**
 * Context interceptor interface.
 * Implementations are responsible for extracting the current user context from the request,
 * including details such as user identity and preferences.
 */
@Slf4j
@Component
public class ContextInterceptor implements HandlerInterceptor {

}
