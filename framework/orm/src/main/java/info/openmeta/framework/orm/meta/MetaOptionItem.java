package info.openmeta.framework.orm.meta;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.orm.model.SysOptionItemTrans;
import lombok.Data;
import org.springframework.util.StringUtils;

import java.io.Serial;
import java.io.Serializable;

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

    /**
     * Get item name by language code from translations.
     * If the translation is not found, return the item name.
     *
     * @return item name
     */
    public String getItemName() {
        String languageCode = ContextHolder.getContext().getLanguage().getCode();
        SysOptionItemTrans itemTrans = MetaTranslationCache.getOptionItemTrans(languageCode, id);
        if (itemTrans == null) {
            return itemName;
        } else {
            String translation = itemTrans.getItemName();
            return StringUtils.hasText(translation) ? translation : itemName;
        }
    }
}