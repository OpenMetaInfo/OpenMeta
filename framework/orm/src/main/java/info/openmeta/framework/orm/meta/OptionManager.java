package info.openmeta.framework.orm.meta;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.jdbc.JdbcService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Global option set cache manager
 */
@Component
@Slf4j
public class OptionManager {

    private static final Map<String, Map<String, MetaOptionItem>> META_OPTION_SET_MAP = new ConcurrentHashMap<>(100);

    @Autowired
    private JdbcService<?> jdbcService;

    /**
     * Initialize the optionSet structure as: {optionSetCode: {itemCode: metaOptionItem}}
     */
    public void init() {
        META_OPTION_SET_MAP.clear();
        // Select all optionItems from the database, and order by optionItem sequence
        List<MetaOptionItem> metaOptionItems = jdbcService.selectMetaEntityList("SysOptionItem", MetaOptionItem.class, "sequence");
        metaOptionItems.forEach(item -> {
            // Update the optionSet cache
            if (META_OPTION_SET_MAP.containsKey(item.getOptionSetCode())) {
                META_OPTION_SET_MAP.get(item.getOptionSetCode()).put(item.getItemCode(), item);
            } else {
                // Use LinkedHashMap to ensure the order of options
                Map<String, MetaOptionItem> map = new LinkedHashMap<>();
                map.put(item.getItemCode(), item);
                META_OPTION_SET_MAP.put(item.getOptionSetCode(), map);
            }
        });
    }

    /**
     * Get the ordered optionItems by optionSetCode
     *
     * @param optionSetCode option set code
     * @return ordered optionItems {itemCode: itemName}
     */
    public static Map<String, String> getMetaOptionItems(String optionSetCode) {
        Assert.isTrue(META_OPTION_SET_MAP.containsKey(optionSetCode),
                "optionSetCode {0} does not exist in OptionSet metadata.", optionSetCode);
        Map<String, String> orderedOptionItems = new LinkedHashMap<>();
        META_OPTION_SET_MAP.get(optionSetCode).values().forEach(sysOptionItem ->
                orderedOptionItems.put(sysOptionItem.getItemCode(), sysOptionItem.getItemName()));
        return orderedOptionItems;
    }

    /**
     * Get the optionItem object by optionSetCode and optionItemCode, return null if not exists.
     *
     * @param optionSetCode option set code
     * @param itemCode option item code
     * @return optionItem object
     */
    public static MetaOptionItem getMetaOptionItem(String optionSetCode, String itemCode) {
        Assert.isTrue(META_OPTION_SET_MAP.containsKey(optionSetCode),
                "optionSetCode {0} does not exist in OptionSet metadata.", optionSetCode);
        return META_OPTION_SET_MAP.get(optionSetCode).get(itemCode);
    }

    /**
     * Get the optionItem name by optionSetCode and optionItemCode, return null if not exists.
     *
     * @param optionSetCode option set code
     * @param itemCode option item code
     * @return optionItem name
     */
    public static String getItemNameByCode(String optionSetCode, String itemCode) {
        MetaOptionItem metaOptionItem = getMetaOptionItem(optionSetCode, itemCode);
        return metaOptionItem == null ? null : metaOptionItem.getItemName();
    }

    /**
     * Get the optionItem code by optionSetCode and optionItemName, return null if not exists.
     *
     * @param optionSetCode option set code
     * @param itemName option item name
     * @return optionItem code
     */
    public static String getItemCodeByName(String optionSetCode, String itemName) {
        for (MetaOptionItem metaOptionItem : META_OPTION_SET_MAP.get(optionSetCode).values()) {
            if (metaOptionItem.getItemName().equals(itemName)) {
                return metaOptionItem.getItemCode();
            }
        }
        return null;
    }

    /**
     * Check if the optionItem exists by optionSetCode and optionItemCode.
     *
     * @param optionSetCode option set code
     * @param itemCode option item code
     * @return true if exists
     */
    public static boolean existsItemCode(String optionSetCode, String itemCode) {
        return META_OPTION_SET_MAP.containsKey(optionSetCode) && META_OPTION_SET_MAP.get(optionSetCode).containsKey(itemCode);
    }
}
