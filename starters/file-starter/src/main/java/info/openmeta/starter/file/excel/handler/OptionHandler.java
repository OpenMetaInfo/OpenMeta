package info.openmeta.starter.file.excel.handler;

import info.openmeta.framework.base.exception.ValidationException;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.OptionManager;
import info.openmeta.starter.file.dto.ImportFieldDTO;
import org.springframework.util.StringUtils;

/**
 * OptionHandler
 * Compatible with the itemName and itemCode of OptionItem.
 */
public class OptionHandler extends BaseImportHandler {

    public OptionHandler(MetaField metaField, ImportFieldDTO importFieldDTO) {
        super(metaField, importFieldDTO);
    }

    /**
     * Handle the option value
     * @param value The option value
     * @return The option itemCode
     */
    public Object handleValue(Object value) {
        if (value instanceof String optionStr && StringUtils.hasText(optionStr)) {
            optionStr = optionStr.trim();
            String optionSetCode = metaField.getOptionSetCode();
            if (OptionManager.existsItemCode(optionSetCode, optionStr)) {
                return optionStr;
            }
            // Treat the option string as itemName
            String optionItemCode = OptionManager.getItemCodeByName(optionSetCode, optionStr);
            if (optionItemCode == null) {
                throw new ValidationException("The option item does not exist {0}", optionStr);
            }
            return optionItemCode;
        } else {
            return value;
        }
    }

}
