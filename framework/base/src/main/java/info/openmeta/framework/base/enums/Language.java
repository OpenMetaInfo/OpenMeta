package info.openmeta.framework.base.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import info.openmeta.framework.base.utils.Assert;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Language enum, encoding in ISO 639-1 format, using `-` as separator.
 */
@Slf4j
@Getter
@AllArgsConstructor
public enum Language {
    AM_ET("am-ET", "Amharic / አምሃርኛ"),
    AR_001("ar-001", "Arabic / الْعَرَبيّة"),
    AR_SY("ar-SY", "Arabic (Syria) / الْعَرَبيّة"),
    AZ_AZ("az-AZ", "Azerbaijani / Azərbaycanca"),
    BG_BG("bg-BG", "Bulgarian / български език"),
    BN_IN("bn-IN", "Bengali / বাংলা"),
    BS_BA("bs-BA", "Bosnian / bosanski jezik"),
    CA_ES("ca-ES", "Catalan / Català"),
    CS_CZ("cs-CZ", "Czech / Čeština"),
    DA_DK("da-DK", "Danish / Dansk"),
    DE_CH("de-CH", "German (CH) / Deutsch (CH)"),
    DE_DE("de-DE", "German / Deutsch"),
    EL_GR("el-GR", "Greek / Ελληνικά"),
    EN_AU("en-AU", "English (AU)"),
    EN_CA("en-CA", "English (CA)"),
    EN_GB("en-GB", "English (UK)"),
    EN_IN("en-IN", "English (IN)"),
    EN_NZ("en-NZ", "English (NZ)"),
    EN_US("en-US", "English (US)"),
    ES_419("es-419", "Spanish (Latin America) / Español (América Latina)"),
    ES_AR("es-AR", "Spanish (AR) / Español (AR)"),
    ES_BO("es-BO", "Spanish (BO) / Español (BO)"),
    ES_CL("es-CL", "Spanish (CL) / Español (CL)"),
    ES_CO("es-CO", "Spanish (CO) / Español (CO)"),
    ES_CR("es-CR", "Spanish (CR) / Español (CR)"),
    ES_DO("es-DO", "Spanish (DO) / Español (DO)"),
    ES_EC("es-EC", "Spanish (EC) / Español (EC)"),
    ES_ES("es-ES", "Spanish / Español"),
    ES_GT("es-GT", "Spanish (GT) / Español (GT)"),
    ES_MX("es-MX", "Spanish (MX) / Español (MX)"),
    ES_PA("es-PA", "Spanish (PA) / Español (PA)"),
    ES_PE("es-PE", "Spanish (PE) / Español (PE)"),
    ES_PY("es-PY", "Spanish (PY) / Español (PY)"),
    ES_UY("es-UY", "Spanish (UY) / Español (UY)"),
    ES_VE("es-VE", "Spanish (VE) / Español (VE)"),
    ET_EE("et-EE", "Estonian / Eesti keel"),
    EU_ES("eu-ES", "Basque / Euskara"),
    FA_IR("fa-IR", "Persian / فارسی"),
    FI_FI("fi-FI", "Finnish / Suomi"),
    FR_BE("fr-BE", "French (BE) / Français (BE)"),
    FR_CA("fr-CA", "French (CA) / Français (CA)"),
    FR_CH("fr-CH", "French (CH) / Français (CH)"),
    FR_FR("fr-FR", "French / Français"),
    GL_ES("gl-ES", "Galician / Galego"),
    GU_IN("gu-IN", "Gujarati / ગુજરાતી"),
    HE_IL("he-IL", "Hebrew / עִבְרִי"),
    HI_IN("hi-IN", "Hindi / हिंदी"),
    HR_HR("hr-HR", "Croatian / hrvatski jezik"),
    HU_HU("hu-HU", "Hungarian / Magyar"),
    ID_ID("id-ID", "Indonesian / Bahasa Indonesia"),
    IT_IT("it-IT", "Italian / Italiano"),
    JA_JP("ja-JP", "Japanese / 日本語"),
    KA_GE("ka-GE", "Georgian / ქართული ენა"),
    KAB_DZ("kab-DZ", "Kabyle / Taqbaylit"),
    KM_KH("km-KH", "Khmer / ភាសាខ្មែរ"),
    KO_KP("ko-KP", "Korean (KP) / 한국어 (KP)"),
    KO_KR("ko-KR", "Korean (KR) / 한국어 (KR)"),
    LB_LU("lb-LU", "Luxembourgish"),
    LO_LA("lo-LA", "Lao / ພາສາລາວ"),
    LT_LT("lt-LT", "Lithuanian / Lietuvių kalba"),
    LV_LV("lv-LV", "Latvian / latviešu valoda"),
    MK_MK("mk-MK", "Macedonian / македонски јазик"),
    ML_IN("ml-IN", "Malayalam / മലയാളം"),
    MN_MN("mn-MN", "Mongolian / монгол"),
    MS_MY("ms-MY", "Malay / Bahasa Melayu"),
    MY_MM("my-MM", "Burmese / ဗမာစာ"),
    NB_NO("nb-NO", "Norwegian Bokmål / Norsk bokmål"),
    NL_BE("nl-BE", "Dutch (BE) / Nederlands (BE)"),
    NL_NL("nl-NL", "Dutch / Nederlands"),
    PL_PL("pl-PL", "Polish / Język polski"),
    PT_AO("pt-AO", "Portuguese (AO) / Português (AO)"),
    PT_BR("pt-BR", "Portuguese (BR) / Português (BR)"),
    PT_PT("pt-PT", "Portuguese / Português"),
    RO_RO("ro-RO", "Romanian / română"),
    RU_RU("ru-RU", "Russian / русский язык"),
    SK_SK("sk-SK", "Slovak / Slovenský jazyk"),
    SL_SI("sl-SI", "Slovenian / slovenščina"),
    SQ_AL("sq-AL", "Albanian / Shqip"),
    SR_LATIN("sr-latin", "Serbian (Latin) / srpski"),
    SR_RS("sr-RS", "Serbian (Cyrillic) / српски"),
    SV_SE("sv-SE", "Swedish / Svenska"),
    TE_IN("te-IN", "Telugu / తెలుగు"),
    TH_TH("th-TH", "Thai / ภาษาไทย"),
    TL_PH("tl-PH", "Tagalog / Filipino"),
    TR_TR("tr-TR", "Turkish / Türkçe"),
    UK_UA("uk-UA", "Ukrainian / українська"),
    VI_VN("vi-VN", "Vietnamese / Tiếng Việt"),
    ZH_CN("zh-CN", "Chinese (Simplified) / 简体中文"),
    ZH_HK("zh-HK", "Chinese (HK)"),
    ZH_TW("zh-TW", "Chinese (Traditional) / 繁體中文"),;

    @JsonValue
    private final String code;
    private final String name;

    /**
     * code map
     */
    private static final Map<String, Language> codeMap = Stream.of(values())
            .collect(Collectors.toMap(Language::getCode, Function.identity()));

    /**
     * Get language item by languageCode, compatible with the format of `_` separator.
     *
     * @param languageCode language code
     * @return Language
     */
    public static Language of(String languageCode) {
        Assert.notBlank(languageCode, "languageCode cannot be empty!");
        Language language = codeMap.get(languageCode);
        if (language == null) {
            languageCode = languageCode.replace("_", "-");
            language = codeMap.get(languageCode);
        }
        if (language == null) {
            log.error("Language code {} is not supported, please check your format.", languageCode);
        }
        return language;
    }

    /**
     * Check if the language exists
     *
     * @param languageCode language code
     * @return boolean
     */
    public static boolean exists(String languageCode) {
        boolean exist = codeMap.containsKey(languageCode);
        if (!exist) {
            languageCode = languageCode.replace("_", "-");
            exist = codeMap.containsKey(languageCode);
        }
        return exist;
    }

}
