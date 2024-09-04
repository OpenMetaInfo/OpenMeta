package info.openmeta.framework.base.context;

import info.openmeta.framework.base.constant.BaseConstant;
import lombok.Data;
import org.springframework.util.StringUtils;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDate;
import java.util.Locale;
import java.util.TimeZone;
import java.util.UUID;

/**
 * Environment parameters of current user.
 */
@Data
public class Context implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    private Long userId;
    private String name;
    private String languageCode = BaseConstant.DEFAULT_LANGUAGE_CODE;
    private TimeZone timeZone;

    private Serializable tenantId;
    private String token;
    private String traceId;

    private UserInfo userInfo;

    /**
     * Whether to skip permission verification (including model permission and data range),
     * the default is to perform permission verification.
     */
    private boolean skipPermissionCheck = false;

    /**
     * Whether to mask field value which maskingType is not null
     */
    private boolean dataMask = false;

    /**
     * Whether to trigger the flow, the default is true.
     * It is allowed to be set to not trigger in specific scenarios,
     * such as batch import and custom Controller, and manually trigger it.
     */
    private boolean triggerFlow = true;

    /**
     * Set by API parameters or @Debug annotation, used to output Debug logs,
     */
    private boolean debug = false;

    /**
     * The effective date specified when querying timeline data, the default is the current date,
     * and can be explicitly passed in the API parameters.
     */
    private LocalDate effectiveDate = LocalDate.now();

    /**
     * Whether to read the primary database in the read-write separation mode
     */
    private boolean readPrimary;

    /**
     * Default constructor, use UUID to fill in when traceId is not specified,
     * used for scenarios such as cron tasks and integration
     */
    public Context() {
        this.traceId = UUID.randomUUID().toString();
    }

    /**
     * @param traceId passed by the client or upstream system
     */
    public Context(String traceId) {
        this.traceId = StringUtils.hasLength(traceId) ? UUID.randomUUID().toString() : traceId;
    }

    public void setEffectiveDate(LocalDate effectiveDate) {
        if (effectiveDate != null) {
            this.effectiveDate = effectiveDate;
        }
    }

    /**
     * Set the language code of the current user.
     *
     * @param locale the locale to get the language code
     */
    public void setLanguageCode(Locale locale) {
        if (locale != null) {
            this.languageCode = locale.toLanguageTag();
        }
    }

    /**
     * Set the language code of the current user.
     * The method is defined to avoid hacking the context language code.
     *
     * @param languageCode the language code to set
     */
    public void setLanguageCode(String languageCode) {
        if (StringUtils.hasLength(languageCode)) {
            this.languageCode = Locale.of(languageCode).toLanguageTag();
        }
    }

    public Context copy() {
        Context newContext = new Context(this.traceId);
        newContext.setUserId(this.userId);
        newContext.setName(this.name);
        newContext.setLanguageCode(this.languageCode);
        newContext.setTimeZone(this.timeZone);
        newContext.setTenantId(this.tenantId);
        newContext.setSkipPermissionCheck(this.skipPermissionCheck);
        newContext.setDataMask(this.dataMask);
        newContext.setTriggerFlow(this.triggerFlow);
        newContext.setDebug(this.debug);
        newContext.setEffectiveDate(this.effectiveDate);
        newContext.setReadPrimary(this.readPrimary);
        return newContext;
    }

}
