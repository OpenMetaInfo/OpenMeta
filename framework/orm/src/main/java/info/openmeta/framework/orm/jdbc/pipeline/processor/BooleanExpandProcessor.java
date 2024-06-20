package info.openmeta.framework.orm.jdbc.pipeline.processor;

import com.google.common.collect.Lists;
import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.MetaOptionItem;
import info.openmeta.framework.orm.meta.OptionManager;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.Map;

/**
 * Boolean field expand processor
 * Expand the boolean field value to the corresponding optionItem name or key-value pair.
 */
@Slf4j
public class BooleanExpandProcessor extends BaseProcessor {

    private final ConvertType convertType;

    /**
     * Field processor object constructor
     *
     * @param metaField field metadata object
     */
    public BooleanExpandProcessor(MetaField metaField, ConvertType convertType) {
        super(metaField);
        this.convertType = convertType;
    }

    /**
     * Boolean field output expansion processing.
     *
     * @param row Single-row output data
     */
    @Override
    public void processOutputRow(Map<String, Object> row) {
        if (row.containsKey(fieldName)) {
            boolean rawValue = Boolean.TRUE.equals(row.get(fieldName));
            String itemCode = Boolean.toString(rawValue);
            row.put(fieldName, getOptionItemName(rawValue, itemCode));
        }
    }

    /**
     * Get the optionItem name of boolean field corresponding to the BOOLEAN_OPTION_CODE.
     *
     * @param rawValue Boolean field original value
     * @param itemCode Boolean itemCode
     * @return According to the convertType, the result is "Yes" or the string list composed of [true, "Yes"].
     */
    public Object getOptionItemName(boolean rawValue, String itemCode) {
        MetaOptionItem metaOptionItem = OptionManager.getMetaOptionItem(BaseConstant.BOOLEAN_OPTION_CODE, itemCode);
        if (ConvertType.KEY_AND_DISPLAY.equals(convertType)) {
            List<Object> valueList = Lists.newArrayList(rawValue, metaOptionItem.getItemName());
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
