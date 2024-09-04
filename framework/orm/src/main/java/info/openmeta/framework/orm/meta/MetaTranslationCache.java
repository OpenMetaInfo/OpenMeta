package info.openmeta.framework.orm.meta;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.jdbc.JdbcService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Data translation cache
 */
@Component
@Slf4j
public class MetaTranslationCache {

    /** Data translation cache: {languageCode: {model: {id: {field: translation}}}} */
    private static final Map<String, Map<String, Map<Serializable, Map<String, String>>>> TRANS_CACHE = new ConcurrentHashMap<>();

    @Autowired
    private JdbcService<?> jdbcService;

    public void init() {
        TRANS_CACHE.clear();
        List<TranslationItem> translations = jdbcService.selectMetaEntityList("SysTranslation", TranslationItem.class, null);
        translations.forEach(translation -> {
            // 获取或创建 languageCode 层
            TRANS_CACHE
                    .computeIfAbsent(translation.getLanguageCode(), k -> new ConcurrentHashMap<>())
                    // 获取或创建 modelName 层
                    .computeIfAbsent(translation.getModelName(), k -> new ConcurrentHashMap<>())
                    // 获取或创建 rowId 层
                    .computeIfAbsent(translation.getRowId(), k -> new ConcurrentHashMap<>())
                    // 设置 fieldName 对应的 translation
                    .put(translation.getFieldName(), translation.getTranslation());
        });
    }

    /**
     * Get the translation of the specified field.
     * return null if not found.
     *
     * @param languageCode language code
     * @param model        model name
     * @param id           row id
     * @param fieldName    field name
     * @return translation
     */
    public static String getTranslation(String languageCode, String model, Serializable id, String fieldName) {
        Assert.notNull(languageCode, "languageCode must not be null");
        Assert.notNull(model, "model must not be null");
        Assert.notNull(id, "id must not be null");
        Assert.notNull(fieldName, "fieldName must not be null");

        return TRANS_CACHE.getOrDefault(languageCode, new HashMap<>())
                .getOrDefault(model, new HashMap<>())
                .getOrDefault(id, new HashMap<>())
                .get(fieldName);
    }
}
