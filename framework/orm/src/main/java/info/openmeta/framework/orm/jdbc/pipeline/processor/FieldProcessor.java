package info.openmeta.framework.orm.jdbc.pipeline.processor;

import java.util.List;
import java.util.Map;

/**
 * Field processor interface
 */
public interface FieldProcessor {

    /**
     * Process a single-row input data.
     *
     * @param row The single-row data to be updated
     */
    void processInputRow(Map<String, Object> row);

    /**
     * Batch process input data
     *
     * @param rows The list of data to be updated
     */
    void batchProcessInputRows(List<Map<String, Object>> rows);

    /**
     * Format a single-row output data.
     *
     * @param row The single-row output data
     */
    void processOutputRow(Map<String, Object> row);

    /**
     * Batch process output data
     *
     * @param rows The list of output data
     */
    void batchProcessOutputRows(List<Map<String, Object>> rows);
}
