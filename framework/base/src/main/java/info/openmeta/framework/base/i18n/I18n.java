package info.openmeta.framework.base.i18n;

import info.openmeta.framework.base.context.ContextHolder;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * I18n static methods to get translation by original content according to current context with or without parameters to format.
 * The original content will be returned if there are no translation defined.
 * The default locale is Locale.US if the context language is not set.
 * Cases:
 *      I18n.get("Hello World!");
 *      I18n.get("Hello World!", Locale.SIMPLIFIED_CHINESE);
 *      I18n.get("Hello World! {0} {1}", firstName, lastName);
 */
@Slf4j
public class I18n {

    /** Language code to translation map: {languageCode: {original: translation}} */
    private static final Map<String, Map<String, String>> MESSAGE_TRANSLATIONS = new ConcurrentHashMap<>();

    /**
     * Get translation by original according to current context with or without parameters to format.
     * The original content will be returned if there are no translations defined.
     *
     * @param original the original text to be translated
     * @param args optional arguments to format the translation
     * @return the translated text, or the original text if no translation is found
     */
    public static String get(String original, Object... args) {
        String languageCode = ContextHolder.getContext().getLanguage().getCode();
        if (MESSAGE_TRANSLATIONS.containsKey(languageCode)) {
            return getByLanguage(languageCode, original, args);
        } else {
            log.warn("Language {} does not has any translation", languageCode);
            return args == null || args.length == 0 ? original : MessageFormat.format(original, args);
        }
    }

    /**
     * Get translation by original according to specified language with or without parameters to format.
     * The original will be treated as the default translation if it has no translation.
     *
     * @param languageCode the language code to get the translation for
     * @param original the original text to be translated
     * @param args optional arguments to format the translation
     * @return the translated text, or the original text if no translation is found
     */
    public static String getByLanguage(String languageCode, String original, Object... args) {
        if (StringUtils.isBlank(original)) {
            return StringUtils.EMPTY;
        }
        String translation = MESSAGE_TRANSLATIONS.get(languageCode).getOrDefault(original, original);
        return args == null || args.length == 0 ? translation : MessageFormat.format(translation, args);
    }

    /**
     * Get translation by original according to specified language with or without parameters to format.
     * The original will be treated as the default translation if it has no translation.
     *
     * @param language the locale to get the translation for
     * @param original the original text to be translated
     * @param args optional arguments to format the translation
     * @return the translated text, or the original text if no translation is found
     */
    public static String getByLanguage(Locale language, String original, Object... args) {
        return getByLanguage(language.getLanguage(), original, args);
    }

    public static void put(String languageCode, List<TranslationItem> translationItems) {
        translationItems.forEach(item -> {
            if (MESSAGE_TRANSLATIONS.containsKey(languageCode)) {
                MESSAGE_TRANSLATIONS.get(languageCode).put(item.getOriginal(), item.getTranslation());
            } else {
                Map<String, String> map = new HashMap<>(64);
                map.put(item.getOriginal(), item.getTranslation());
                MESSAGE_TRANSLATIONS.put(languageCode, map);
            }
        });
    }
}
