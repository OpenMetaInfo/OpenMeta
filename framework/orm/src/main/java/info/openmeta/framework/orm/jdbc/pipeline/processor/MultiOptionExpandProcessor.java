package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.base.utils.Cast;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * MultiOption field processor.
 * When CREATE or UPDATE, the List<String> is converted to String for storage by `MultiStringProcessor`.
 * When READ data, the String is converted to List<String> firstly, and after the computed field is executed,
 * the option list is expanded according to the config of `flexQuery.convertType`.
 */
public class MultiOptionExpandProcessor extends OptionExpandProcessor {

    /**
     * Constructor of MultiOptionExpandProcessor, for processing output data.
     *
     * @param metaField Field metadata object
     * @param accessType Access type
     * @param convertType Convert type
     */
    public MultiOptionExpandProcessor(MetaField metaField, AccessType accessType, ConvertType convertType) {
        super(metaField, accessType, convertType);
    }

    /**
     * Convert the MultiOption field output string to Value List.
     *
     * @param row Single-row output data
     */
    @Override
    public void processOutputRow(Map<String, Object> row) {
        if (row.containsKey(fieldName)) {
            List<String> itemCodes = Cast.of(row.get(fieldName));
            List<Object> itemValues = itemCodes.stream().map(this::getOptionItemValue).collect(Collectors.toList());
            if (ConvertType.DISPLAY.equals(convertType)) {
                String value = itemValues.stream().map(Object::toString).collect(Collectors.joining(", "));
                row.put(fieldName, value);
            } else {
                row.put(fieldName, itemValues);
            }
        }
    }

}
