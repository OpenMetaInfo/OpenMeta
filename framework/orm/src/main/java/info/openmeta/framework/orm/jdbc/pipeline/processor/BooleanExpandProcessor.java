package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.MetaOptionItem;
import info.openmeta.framework.orm.meta.OptionManager;
import lombok.extern.slf4j.Slf4j;

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
     * @param accessType access type
     */
    public BooleanExpandProcessor(MetaField metaField, AccessType accessType, ConvertType convertType) {
        super(metaField, accessType);
        this.convertType = convertType;
    }

    /**
     * Batch processing of output rows
     *
     * @param rows List of rows to be processed
     */
    @Override
    public void batchProcessOutputRows(List<Map<String, Object>> rows) {
        if (ConvertType.DISPLAY.equals(convertType)
                && OptionManager.existsOptionSetCode(BaseConstant.BOOLEAN_OPTION_SET_CODE)) {
            rows.forEach(this::processOutputRow);
        }
    }

    /**
     * Boolean field output expansion processing.
     * Get the optionItem name of boolean field corresponding to the BOOLEAN_OPTION_CODE.
     *
     * @param row Single-row output data
     */
    @Override
    public void processOutputRow(Map<String, Object> row) {
        if (row.containsKey(fieldName)) {
            boolean rawValue = Boolean.TRUE.equals(row.get(fieldName));
            String itemCode = Boolean.toString(rawValue);
            MetaOptionItem metaOptionItem = OptionManager.getMetaOptionItem(BaseConstant.BOOLEAN_OPTION_SET_CODE, itemCode);
            row.put(fieldName, metaOptionItem.getItemName());
        }
    }
}
