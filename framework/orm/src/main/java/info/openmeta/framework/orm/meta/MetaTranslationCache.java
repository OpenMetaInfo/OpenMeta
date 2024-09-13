package info.openmeta.framework.orm.meta;

import info.openmeta.framework.orm.jdbc.JdbcService;
import info.openmeta.framework.orm.model.SysFieldTrans;
import info.openmeta.framework.orm.model.SysOptionItemTrans;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Metadata translation cache
 */
@Slf4j
@Component
public class MetaTranslationCache {

    /** Metadata translation cache: {languageCode: {id: TranslationObject}} */
    private static final Map<String, Map<Long, SysFieldTrans>> FIELD_TRANS_CACHE = new ConcurrentHashMap<>();
    private static final Map<String, Map<Long, SysOptionItemTrans>> OPTION_ITEM_TRANS_CACHE = new ConcurrentHashMap<>();

    @Autowired
    private JdbcService<?> jdbcService;

    /**
     * Initialize the translation cache.
     */
    public void init() {
        // Load field translations
        FIELD_TRANS_CACHE.clear();
        List<SysFieldTrans> fieldTransList = jdbcService.selectMetaEntityList(SysFieldTrans.class, null);
        Map<String, Map<Long, SysFieldTrans>> fieldTransMap = fieldTransList.stream()
                .collect(Collectors.groupingBy(SysFieldTrans::getLanguageCode,
                        Collectors.toMap(SysFieldTrans::getRowId, Function.identity())));
        FIELD_TRANS_CACHE.putAll(fieldTransMap);
        // Load option item translations
        OPTION_ITEM_TRANS_CACHE.clear();
        List<SysOptionItemTrans> optionTransList = jdbcService.selectMetaEntityList(SysOptionItemTrans.class, null);
        Map<String, Map<Long, SysOptionItemTrans>> optionItemTransMap = optionTransList.stream()
                .collect(Collectors.groupingBy(SysOptionItemTrans::getLanguageCode,
                        Collectors.toMap(SysOptionItemTrans::getRowId, Function.identity())));
        OPTION_ITEM_TRANS_CACHE.putAll(optionItemTransMap);
    }

    /**
     * Get the translation of the specified field id.
     * return null if not found.
     *
     * @param languageCode language code
     * @param id           row id
     * @return translation
     */
    public static SysFieldTrans getFieldTrans(String languageCode, Long id) {
        if (FIELD_TRANS_CACHE.containsKey(languageCode)) {
            return FIELD_TRANS_CACHE.get(languageCode).get(id);
        } else {
            return null;
        }
    }

    /**
     * Get the translation of the specified option item id.
     * return null if not found.
     *
     * @param languageCode language code
     * @param id           row id
     * @return translation
     */
    public static SysOptionItemTrans getOptionItemTrans(String languageCode, Long id) {
        if (OPTION_ITEM_TRANS_CACHE.containsKey(languageCode)) {
            return OPTION_ITEM_TRANS_CACHE.get(languageCode).get(id);
        } else {
            return null;
        }
    }
}
