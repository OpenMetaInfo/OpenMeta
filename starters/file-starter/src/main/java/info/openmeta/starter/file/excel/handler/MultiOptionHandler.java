package info.openmeta.starter.file.excel.handler;

import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.OptionManager;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * MultiOptionHandler
 * Compatible with the itemName and itemCode of OptionItem.
 */
public class MultiOptionHandler extends BaseImportHandler {

    public MultiOptionHandler(MetaField metaField) {
        super(metaField);
    }

    /**
     * Handle the MultiOption value
     * @param value The MultiOption value
     * @return The MultiOption items
     */
    public Object handleValue(Object value) {
        if (value instanceof String multiOptionStr && StringUtils.hasText(multiOptionStr)) {
            String optionSetCode = metaField.getOptionSetCode();
            String[] optionList = StringUtils.split(multiOptionStr.trim(), ",");
            List<String> codeList = new ArrayList<>();
            for (String optionStr : optionList) {
                if (OptionManager.existsItemCode(optionSetCode, optionStr)) {
                    codeList.add(optionStr);
                } else {
                    // Treat the option string as itemName
                    String optionItemCode = OptionManager.getItemCodeByName(optionSetCode, optionStr);
                    if (optionItemCode == null) {
                        throw new IllegalArgumentException("The option item does not exist {0}", optionStr);
                    }
                    codeList.add(optionItemCode);
                }
            }
            return codeList;
        } else {
            return value;
        }
    }

}
