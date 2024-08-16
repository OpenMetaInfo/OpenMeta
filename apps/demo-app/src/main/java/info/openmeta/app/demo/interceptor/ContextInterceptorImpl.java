package info.openmeta.app.demo.interceptor;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.constant.RedisConstant;
import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.context.UserInfo;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.web.interceptor.ContextInterceptor;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.framework.web.service.CacheService;
import info.openmeta.framework.web.utils.CookieUtils;
import jakarta.annotation.Nonnull;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.server.ServerWebExchange;

import java.io.IOException;
import java.time.ZoneId;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Extracting the current user context from the request, including details such as user identity and preferences.
 */
@Slf4j
@Component
public class ContextInterceptorImpl implements ContextInterceptor {

    @Autowired
    private CacheService cacheService;

    /**
     * Login address, default to `/login`
     */
    @Value("${system.login.url:/login}")
    private String loginUrl;

    /**
     * Context interceptor that handles session validation and user context setup before request processing.
     * Redirects to login page if the session is invalid or user info is missing.
     *
     * @param request  the current HTTP request
     * @param response the HTTP response
     * @param handler  the object that handles the HTTP request
     * @return boolean indicating whether the request should proceed further
     */
    @Override
    public boolean preHandle(@Nonnull HttpServletRequest request,
                             @Nonnull HttpServletResponse response,
                             @Nonnull Object handler) {
        String sessionId = CookieUtils.getCookie(request, BaseConstant.SESSION_ID);
        if (sessionId == null) {
            // If sessionId is not found in cookies, get it from the request header
            sessionId = request.getHeader(BaseConstant.SESSION_ID);
        }
        boolean redirectToLogin = false;
        if (sessionId == null) {
            // If no sessionId is provided, log an error and redirect to the login page
            log.warn("ContextInterceptorImpl.preHandle: sessionId is null");
            redirectToLogin = true;
        }
        Long userId = cacheService.get(RedisConstant.SESSION + sessionId, Long.class);
        if (userId == null) {
            // If sessionId is invalid, redirect to the login page
            redirectToLogin = true;
        }
        UserInfo userInfo = cacheService.get(RedisConstant.USER_INFO + userId, UserInfo.class);
        if (userInfo == null) {
            // If userInfo does not exist, redirect to the login page
            redirectToLogin = true;
        }
        if (redirectToLogin) {
            redirectLoginResponse(response);
            return false;
        }
        setupUserContext(request, userInfo);
        return true;
    }

    /**
     * Setup user context with user info.
     * Extract the `debug` parameter from the URI to enable debug mode.
     *
     * @param request the current HTTP request
     * @param userInfo the user info
     */
    @Override
    public void setupUserContext(HttpServletRequest request, UserInfo userInfo) {
        Context context = new Context(request.getHeader("traceId"));
        context.setUserId(userInfo.getId());
        context.setName(userInfo.getName());
        context.setLanguage(Locale.of(userInfo.getLanguage()));
        context.setTimeZone(TimeZone.getTimeZone(ZoneId.of(userInfo.getTimezone())));
        context.setTenantId(userInfo.getTenantId());
        context.setUserInfo(userInfo);

        // If the `debug` parameter appears in the URI, enable debug mode in the context.
        String debug = request.getParameter(BaseConstant.DEBUG);
        if (Boolean.parseBoolean(debug) || "1".equals(debug)) {
            context.setDebug(true);
        }
        ContextHolder.setContext(context);
    }

    /**
     * Setup context for anonymous users or requests that do not require permission check.
     * Extract language from request headers or query params, and timezone from customized request headers.
     *
     * @param exchange the server exchange for reactive requests
     */
    public void setupAnonymousContext(ServerWebExchange exchange) {
        Context context = new Context();
        HttpHeaders headers = exchange.getRequest().getHeaders();
        String language = headers.getFirst("Accept-Language");
        if (!StringUtils.hasText(language)) {
            language = exchange.getRequest().getQueryParams().getFirst("language");
        }
        if (StringUtils.hasText(language)) {
            context.setLanguage(Locale.forLanguageTag(language));
        }
        String timezone = headers.getFirst("X-Timezone");
        if (StringUtils.hasText(timezone)) {
            context.setTimeZone(TimeZone.getTimeZone(ZoneId.of(timezone)));
        }
        ContextHolder.setContext(context);
    }

    /**
     * Set response as a JSON response body that contains redirection information to login,
     * enabling client custom redirection.
     *
     * @param response the HTTP response
     */
    private void redirectLoginResponse(HttpServletResponse response) {
        try {
            ApiResponse<String> redirectResponse = ApiResponse.redirect(loginUrl);
            String jsonResponse = JsonMapper.objectToString(redirectResponse);
            response.setStatus(HttpServletResponse.SC_OK);
            response.setContentType(MediaType.APPLICATION_JSON_VALUE);
            response.getWriter().write(jsonResponse);
        } catch (IOException e) {
            log.error("ContextInterceptorImpl.redirectLogin", e);
        }
    }

}
