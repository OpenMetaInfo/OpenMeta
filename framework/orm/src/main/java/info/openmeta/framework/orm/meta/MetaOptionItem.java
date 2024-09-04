package info.openmeta.framework.orm.meta;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.orm.model.SysOptionItemTrans;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.Map;

/**
 * MetaOptionItem
 */
@Data
public class MetaOptionItem implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private Long id;

    private Long appId;

    private Long optionSetId;

    private String optionSetCode;

    private Integer sequence;

    private String itemCode;

    private String itemName;

    private String parentItemCode;

    private String itemColor;

    private String description;

    // {languageCode: SysOptionItemTrans}
    private Map<String, SysOptionItemTrans> translations;

    /**
     * Get item name by language code from translations.
     * If the translation is not found, return the item name.
     *
     * @return item name
     */
    public String getItemName() {
        String languageCode = ContextHolder.getContext().getLanguage().getCode();
        if (translations != null && translations.containsKey(languageCode)) {
            return translations.get(languageCode).getItemName();
        } else {
            return itemName;
        }
    }
}