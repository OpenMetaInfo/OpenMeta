package info.openmeta.framework.base.i18n;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.utils.Assert;
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
public class I18n {

    /** Language code to translation map: {languageCode: {original: translation}} */
    private static final Map<String, Map<String, String>> MESSAGE_TRANSLATIONS = new ConcurrentHashMap<>();

    /**
     * Get translation by original according to current context with or without parameters to format.
     */
    public static String get(String original, Object... args){
        Context context = ContextHolder.getContext();
        if (context == null || BaseConstant.DEFAULT_LANGUAGE.equals(context.getLanguage())) {
            return MessageFormat.format(original, args);
        } else {
            return get(context.getLanguage(), original, args);
        }
    }

    /**
     * Get translation by original according to specified language with or without parameters to format.
     * The original will be treated as the default translation if it has no translation.
     */
    public static String get(Locale language, String original, Object... args){
        String languageCode = language.getLanguage();
        Assert.isTrue(MESSAGE_TRANSLATIONS.containsKey(languageCode), "Language not supported: {0}", languageCode);
        if (StringUtils.isBlank(original)) {
            return StringUtils.EMPTY;
        }
        String translation =  MESSAGE_TRANSLATIONS.get(languageCode).getOrDefault(original, original);
        return MessageFormat.format(translation, args);
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
