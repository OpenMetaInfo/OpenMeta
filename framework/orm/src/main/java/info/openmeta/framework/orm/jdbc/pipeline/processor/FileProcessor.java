package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.meta.MetaField;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;

/**
 * File field processor
 */
@Slf4j
public class FileProcessor extends BaseProcessor {

    public FileProcessor(MetaField metaField, AccessType accessType) {
        super(metaField, accessType);
    }

    /**
     * File field output expansion processing.
     *
     * @param row Single-row output data
     */
    @Override
    public void processOutputRow(Map<String, Object> row) {
        if (!row.containsKey(fieldName)) {
            return;
        }
        // TODO: File field output expansion processing
    }

}
