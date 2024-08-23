package info.openmeta.app.demo.config;

import com.google.common.collect.Lists;
import info.openmeta.framework.web.interceptor.ContextInterceptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import java.util.List;

/**
 * Web Config
 */
@Configuration
public class WebConfig implements WebMvcConfigurer {

    @Autowired
    private ContextInterceptor contextInterceptor;

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        // Exclude Swagger resources and interfaces
        List<String> excludePathPatterns = Lists.newArrayList(
                "/**/v3/api-docs/**",
                "/**/swagger-ui/**",
                "/**/swagger-ui.html",
                "/**/login"
        );
        // Add the Context interceptor to inject user environment variables
        registry.addInterceptor(contextInterceptor)
                .addPathPatterns("/**")
                .excludePathPatterns(excludePathPatterns)
                .order(Ordered.HIGHEST_PRECEDENCE);
    }
}

