package info.openmeta.framework.orm.meta;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.jdbc.JdbcService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Data translation cache
 */
@Component
@Slf4j
public class DataTranslationCache {

    /** Language code to translation map: {languageCode: {original: translation}} */
    private static final Map<String, Map<String, String>> DATA_TRANSLATION_CACHE = new ConcurrentHashMap<>();

    @Autowired
    private JdbcService<?> jdbcService;

    public void init() {
        DATA_TRANSLATION_CACHE.clear();
        List<MetaTranslation> translations = jdbcService.selectMetaEntityList(MetaTranslation.class, null);
        translations.forEach(item -> {
            if (DATA_TRANSLATION_CACHE.containsKey(item.getLanguageCode())) {
                DATA_TRANSLATION_CACHE.get(item.getLanguageCode()).put(item.getOriginal(), item.getTranslation());
            } else {
                Map<String, String> map = new HashMap<>();
                map.put(item.getOriginal(), item.getTranslation());
                DATA_TRANSLATION_CACHE.put(item.getLanguageCode(), map);
            }
        });
    }

    public static String getTranslation(String languageCode, String source){
        Assert.isTrue(DATA_TRANSLATION_CACHE.containsKey(languageCode), "Language {0} not supported.", languageCode);
        if (StringUtils.isBlank(source)) {
            return StringUtils.EMPTY;
        }
        return DATA_TRANSLATION_CACHE.get(languageCode).getOrDefault(source, source);
    }
}
