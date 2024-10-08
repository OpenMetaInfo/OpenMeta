package info.openmeta.framework.web.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import info.openmeta.framework.base.constant.TimeConstant;
import jakarta.annotation.Nonnull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.annotation.Configuration;
import org.springframework.format.FormatterRegistry;
import org.springframework.format.datetime.standard.DateTimeFormatterRegistrar;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.validation.Validator;
import org.springframework.validation.beanvalidation.LocalValidatorFactoryBean;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import java.util.List;

/**
 * Base Web Config
 */
@Configuration
public class BaseWebConfig implements WebMvcConfigurer {

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private MessageSource messageSource;

    /**
     * Define the validation message as key and get the translation.
     */
    @Override
    public Validator getValidator() {
        LocalValidatorFactoryBean validator = new LocalValidatorFactoryBean();
        validator.setValidationMessageSource(messageSource);
        return validator;
    }

    /**
     * Serializes objects using Jackson.
     *
     * @param converters converters
     */
    @Override
    public void configureMessageConverters(List<HttpMessageConverter<?>> converters) {
        converters.addFirst(new MappingJackson2HttpMessageConverter(objectMapper));
        // Add ByteArrayHttpMessageConverter to the first, to fix with the issue of springdoc/swagger-ui
        converters.addFirst(new ByteArrayHttpMessageConverter());
    }

    /**
     * Handles formatting during the data binding process in the Spring MVC framework.
     * Allows passing String request parameters and serializes them into LocalDateTime, LocalDate, and LocalTime.
     *
     * @param registry the formatter registry
     */
    @Override
    public void addFormatters(@Nonnull FormatterRegistry registry) {
        DateTimeFormatterRegistrar registrar = new DateTimeFormatterRegistrar();
        registrar.setDateTimeFormatter(TimeConstant.DATETIME_FORMATTER);
        registrar.setDateFormatter(TimeConstant.DATE_FORMATTER);
        registrar.setTimeFormatter(TimeConstant.TIME_FORMATTER);
        registrar.registerFormatters(registry);
    }

    /**
     * Enable access to Swagger static resources.
     * @param registry registry
     */
    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("/webjars/**").addResourceLocations("classpath:/META-INF/resources/webjars/");
        registry.addResourceHandler("/doc.html").addResourceLocations("classpath:/META-INF/resources/");
    }

}

