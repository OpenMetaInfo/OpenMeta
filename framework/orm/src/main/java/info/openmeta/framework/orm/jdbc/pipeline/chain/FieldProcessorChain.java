package info.openmeta.framework.orm.jdbc.pipeline.chain;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.jdbc.pipeline.processor.FieldProcessor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Field processor chain, process field data according to the chain of responsibility
 */
public class FieldProcessorChain {
    private final List<FieldProcessor> processors = new ArrayList<>();

    public void addProcessor(FieldProcessor fieldProcessor) {
        if (fieldProcessor != null) {
            processors.add(fieldProcessor);
        }
    }

    /**
     * Execute the processors in the responsibility chain to process the input data.
     */
    public void processInputRows(List<Map<String, Object>> rows, AccessType accessType) {
        processors.forEach(p -> p.batchProcessInputRows(rows, accessType));
    }


    /**
     * Execute the processors in the responsibility chain to process the output data.
     */
    public void processOutputRows(List<Map<String, Object>> rows) {
        processors.forEach(p -> p.batchProcessOutputRows(rows));
    }
}
