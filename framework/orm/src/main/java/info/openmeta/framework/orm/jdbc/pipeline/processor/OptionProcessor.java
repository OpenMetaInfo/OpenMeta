package info.openmeta.framework.orm.jdbc.pipeline.processor;

import com.google.common.collect.Lists;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.MetaOptionItem;
import info.openmeta.framework.orm.meta.OptionManager;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * Option field processor
 */
@Slf4j
public class OptionProcessor extends BaseProcessor {

    protected final ConvertType convertType;

    public OptionProcessor(MetaField metaField, ConvertType convertType) {
        super(metaField);
        this.convertType = convertType;
        Assert.notBlank(metaField.getOptionSetCode(),
                "Model field {0}: {1} is a `Option` field, but the `optionSetCode` is not specified!",
                metaField.getModelName(), metaField.getFieldName());
    }

    /**
     * Option field output expansion processing.
     * Convert the optionItemCode to optionItemValue or [code, value, color].
     *
     * @param row Single-row output data
     */
    @Override
    public void processOutputRow(Map<String, Object> row) {
        if (row.containsKey(fieldName)) {
            row.put(fieldName, getOptionItemValue((String) row.get(fieldName)));
        }
    }

    /**
     * Get the item value corresponding to the Option itemCode.
     * The result is a string or a string list composed of [itemCode, itemName] according to the convertType.
     *
     * @param itemCode Option itemCode
     * @return itemName or [itemCode, itemName] according to the convertType.
     */
    public Object getOptionItemValue(String itemCode) {
        String optionSetCode = metaField.getOptionSetCode();
        if (StringUtils.isBlank(itemCode)) {
            return ConvertType.KEY_AND_DISPLAY.equals(convertType) ? new ArrayList<>(0) : "";
        }
        MetaOptionItem metaOptionItem = OptionManager.getMetaOptionItem(optionSetCode, itemCode);
        if (metaOptionItem == null) {
            log.error("""
                    Model field {}: {} is a Option field, but the itemCode `{}` doesn't exist in option set {}.
                    using the itemCode instead of ItemName!""",
                    metaField.getModelName(), metaField.getFieldName(), itemCode, optionSetCode);
            return ConvertType.KEY_AND_DISPLAY.equals(convertType) ? Arrays.asList(itemCode, itemCode) : itemCode;
        } else if (ConvertType.KEY_AND_DISPLAY.equals(convertType)) {
            List<String> valueList = Lists.newArrayList(itemCode, metaOptionItem.getItemName());
            if (StringUtils.isNotBlank(metaOptionItem.getItemColor())) {
                valueList.add(metaOptionItem.getItemColor());
            }
            return valueList;
        } else if (ConvertType.DISPLAY.equals(convertType)) {
            return metaOptionItem.getItemName();
        }
        return itemCode;
    }

}
