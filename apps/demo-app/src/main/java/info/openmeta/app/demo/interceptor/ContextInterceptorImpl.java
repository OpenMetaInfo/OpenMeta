package info.openmeta.app.demo.interceptor;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.constant.RedisConstant;
import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.context.UserInfo;
import info.openmeta.framework.base.enums.Language;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.orm.datasource.DataSourceContextHolder;
import info.openmeta.framework.orm.enums.DynamicDataSourceMode;
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
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.io.IOException;
import java.time.ZoneId;
import java.util.Optional;
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
     * Enable authentication
     * Only used in the demo app
     */
    @Value("${system.enable-auth:true}")
    private boolean enableAuth;

    @Value("${system.multi-tenancy.enable:false}")
    private boolean multiTenancy;

    @Value("${spring.datasource.dynamic.mode:}")
    private DynamicDataSourceMode dynamicDataSourceMode;

    /**
     * Login address, default to `/login`
     */
    @Value("${system.login.url:/login}")
    private String loginUrl;

    @Value("${system.default-language:}")
    private String defaultLanguage;

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
        // Should be removed in non-demo apps
        if (!enableAuth) {
            // If authentication is disabled, set up an anonymous context
            setupAnonymousContext(request);
            return true;
        }
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
        String traceId = Optional.ofNullable(request.getHeader(BaseConstant.TRACE_ID))
                .orElse(Optional.ofNullable(request.getHeader(BaseConstant.X_TRACE_ID))
                        .orElse(request.getHeader(BaseConstant.X_B3_TRACEID)));
        Context context = new Context(traceId);
        context.setUserId(userInfo.getId());
        context.setName(userInfo.getName());
        Language language = this.getCurrentLanguage(request, userInfo.getLanguage());
        context.setLanguage(language);
        context.setTimeZone(TimeZone.getTimeZone(ZoneId.of(userInfo.getTimezone())));
        context.setUserInfo(userInfo);
        if (multiTenancy) {
            this.setMultiTenancyEnv(context, userInfo);
        }
        this.setDebugModeFromRequest(request, context);
        ContextHolder.setContext(context);
    }

    /**
     * Setup context for anonymous users or requests that do not require permission check.
     * Extract language from request headers or query params, and timezone from customized request headers.
     *
     * @param request  the current HTTP request
     */
    public void setupAnonymousContext(HttpServletRequest request) {
        Context context = new Context();
        Language language = this.getCurrentLanguage(request, null);
        context.setLanguage(language);
        String timezone = request.getHeader("X-Timezone");
        if (StringUtils.hasText(timezone)) {
            context.setTimeZone(TimeZone.getTimeZone(ZoneId.of(timezone)));
        }
        this.setDebugModeFromRequest(request, context);
        ContextHolder.setContext(context);
    }

    /**
     * Extract language from user info, query params, request headers or default language.
     * LanguageCode from the URI params will override the language from the request headers.
     * For example, `?language=zh-CN` will set the language to Chinese.
     * request.getLocale() will be used if no language is specified, which is based on the Accept-Language header.
     * `zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7` will be parsed as `zh-CN`, which is the highest priority language.
     *
     * @param request the current HTTP request
     * @return the languageCode extracted from the request
     */
    private Language getCurrentLanguage(HttpServletRequest request, String userLanguage) {
        if (StringUtils.hasText(userLanguage)) {
            return Language.of(userLanguage);
        }
        String languageCode = request.getParameter("language");
        if (StringUtils.hasText(languageCode)) {
            return Language.of(userLanguage);
        } else if (StringUtils.hasText(request.getHeader("Accept-Language"))) {
            languageCode = request.getLocale().toLanguageTag();
            return Language.of(languageCode);
        } else if (StringUtils.hasText(defaultLanguage)) {
            return Language.of(defaultLanguage);
        }
        return null;
    }

    /**
     * Extract the `debug` parameter from the URI to enable debug mode.
     *
     * @param request the current HTTP request
     * @param context the current context
     */
    private void setDebugModeFromRequest(HttpServletRequest request, Context context) {
        String debug = request.getParameter(BaseConstant.DEBUG);
        if (Boolean.parseBoolean(debug) || "1".equals(debug)) {
            context.setDebug(true);
        }
    }

    /**
     * Set the datasource key for the current thread based on the user info.
     * Used for multi-tenancy applications, the mode of shared app with separate data.
     *
     * @param context the current context
     * @param userInfo the user info
     */
    private void setMultiTenancyEnv(Context context, UserInfo userInfo) {
        Assert.notBlank(userInfo.getTenantId(), "User tenantId cannot be null in multi-tenancy mode.");
        context.setTenantId(userInfo.getTenantId());
        if (DynamicDataSourceMode.MULTI_TENANCY_ISOLATED.equals(dynamicDataSourceMode)) {
            Assert.notBlank(userInfo.getDatasourceKey(),
                    "User datasourceKey cannot be blank in isolated database multi-tenancy mode.");
            // Set the datasource key for the current thread
            DataSourceContextHolder.setDataSourceKey(userInfo.getDatasourceKey());
        }
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
