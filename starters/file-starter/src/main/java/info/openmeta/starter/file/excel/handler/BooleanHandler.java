package info.openmeta.starter.file.excel.handler;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.exception.ValidationException;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.OptionManager;
import info.openmeta.starter.file.dto.ImportFieldDTO;
import org.springframework.util.StringUtils;

/**
 * BooleanHandler
 * Compatible with the itemName and itemCode of OptionItem.
 */
public class BooleanHandler extends BaseImportHandler {

    public BooleanHandler(MetaField metaField, ImportFieldDTO importFieldDTO) {
        super(metaField, importFieldDTO);
    }

    /**
     * Handle the Object value
     * @param value The Object value
     * @return The Boolean value
     */
    public Object handleValue(Object value) {
        if (value instanceof String valueStr && StringUtils.hasText(valueStr)) {
            valueStr = valueStr.trim().toLowerCase();
            String optionSetCode = BaseConstant.BOOLEAN_OPTION_SET_CODE;
            if (OptionManager.existsItemCode(optionSetCode, valueStr)) {
                return Boolean.valueOf(valueStr);
            } else {
                // Treat the boolean string as itemName
                String optionItemCode = OptionManager.getItemCodeByName(optionSetCode, valueStr);
                if (optionItemCode == null) {
                    throw new ValidationException("The Boolean field `{0}` is incorrect `{1}`", labelName, valueStr);
                }
                return Boolean.valueOf(optionItemCode);
            }
        } else {
            return value;
        }
    }

}
