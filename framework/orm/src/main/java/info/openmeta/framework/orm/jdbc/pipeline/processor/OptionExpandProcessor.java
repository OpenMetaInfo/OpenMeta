package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.MetaOptionItem;
import info.openmeta.framework.orm.meta.OptionManager;
import info.openmeta.framework.orm.vo.OptionReference;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.Map;

/**
 * Option field processor
 */
@Slf4j
public class OptionExpandProcessor extends BaseProcessor {

    protected ConvertType convertType;

    public OptionExpandProcessor(MetaField metaField, AccessType accessType, ConvertType convertType) {
        super(metaField, accessType);
        Assert.notBlank(metaField.getOptionSetCode(),
                "Model field {0}: {1} is a `Option` field, but the `optionSetCode` is not specified!",
                metaField.getModelName(), metaField.getFieldName());
        this.convertType = convertType;
    }

    /**
     * Option field output expansion processing.
     * Convert the optionItemCode to optionItemValue or OptionReference object.
     *
     * @param row Single-row output data
     */
    @Override
    public void processOutputRow(Map<String, Object> row) {
        if (row.containsKey(fieldName)) {
            row.put(fieldName, getOptionItemValue(row.get(fieldName) + ""));
        }
    }

    /**
     * Get the item value corresponding to the Option itemCode.
     * The result is a string or an OptionReference object according to the convertType.
     *
     * @param itemCode Option itemCode
     * @return itemName or OptionReference object according to the convertType.
     */
    public Object getOptionItemValue(String itemCode) {
        String optionSetCode = metaField.getOptionSetCode();
        if (StringUtils.isBlank(itemCode)) {
            return ConvertType.REFERENCE.equals(convertType) ? null : "";
        }
        MetaOptionItem metaOptionItem = OptionManager.getMetaOptionItem(optionSetCode, itemCode);
        if (metaOptionItem == null) {
            log.error("""
                    Model field {}: {} is a Option field, but the itemCode `{}` doesn't exist in option set {}.
                    using the itemCode instead of ItemName!""",
                    metaField.getModelName(), metaField.getFieldName(), itemCode, optionSetCode);
            return ConvertType.REFERENCE.equals(convertType) ?
                    OptionReference.of(itemCode, itemCode) : itemCode;
        } else if (ConvertType.REFERENCE.equals(convertType)) {
            return OptionReference.of(itemCode, metaOptionItem.getItemName(), metaOptionItem.getItemColor());
        } else if (ConvertType.DISPLAY.equals(convertType)) {
            return metaOptionItem.getItemName();
        }
        return itemCode;
    }

}
