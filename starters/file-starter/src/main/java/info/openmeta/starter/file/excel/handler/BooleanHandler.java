package info.openmeta.starter.file.excel.handler;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.OptionManager;
import org.springframework.util.StringUtils;

/**
 * BooleanHandler
 * Compatible with the itemName and itemCode of OptionItem.
 */
public class BooleanHandler extends BaseImportHandler {

    public BooleanHandler(MetaField metaField) {
        super(metaField);
    }

    /**
     * Handle the Object value
     * @param value The Object value
     * @return The Boolean value
     */
    public Object handleValue(Object value) {
        if (value instanceof String valueStr && StringUtils.hasText(valueStr)) {
            valueStr = valueStr.trim().toLowerCase();
            String optionSetCode = BaseConstant.BOOLEAN_OPTION_CODE;
            if (OptionManager.existsItemCode(optionSetCode, valueStr)) {
                return Boolean.valueOf(valueStr);
            } else {
                // Treat the boolean string as itemName
                String optionItemCode = OptionManager.getItemCodeByName(optionSetCode, valueStr);
                if (optionItemCode == null) {
                    throw new IllegalArgumentException("The Boolean item is incorrect `{0}`", valueStr);
                }
                return Boolean.valueOf(optionItemCode);
            }
        } else {
            return value;
        }
    }

}
