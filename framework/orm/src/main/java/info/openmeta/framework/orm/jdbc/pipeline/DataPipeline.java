package info.openmeta.framework.orm.jdbc.pipeline;

import info.openmeta.framework.orm.jdbc.pipeline.chain.FieldProcessorChain;
import lombok.Getter;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * The abstract class DataPipeline for processing model data.
 */
public abstract class DataPipeline {

    protected final String modelName;

    // Get the extracted field set
    @Getter
    protected Set<String> fields;

    protected FieldProcessorChain processorChain;

    protected DataPipeline(String modelName) {
        this.modelName = modelName;
    }

    // Build field processing chain
    public abstract FieldProcessorChain buildFieldProcessorChain();

    /**
     * Process the rows data for Read, including batch decryption and data format conversion.
     * Where, when processing related ManyToOne, OneToOne, and cascaded fields, without permission check.
     *
     * @param rows rows data
     */
    public void processReadData(List<Map<String, Object>> rows) {}

    /**
     * Process the rows data for Create, including batch encryption, data format conversion,
     * default value assignment, and length, required, read-only, etc. attribute verification.
     * Where, when processing relation fields and cascaded fields, without permission check.
     *
     * @param rows rows data
     * @param createdTime creation time
     * @return processed data
     */
    public List<Map<String, Object>> processCreateData(List<Map<String, Object>> rows, LocalDateTime createdTime) {
        return Collections.emptyList();
    }

    /**
     * Process the rows data for Update, including batch encryption, data format conversion,
     * default value assignment, and length, required, read-only, etc. attribute verification.
     * Where, when processing relation fields and cascaded fields, without permission check.
     *
     * @param rows rows data
     * @param originalRowsMap original rows data map
     * @param updatedTime update time
     * @return processed data
     */
    public List<Map<String, Object>> processUpdateData(List<Map<String, Object>> rows, Map<Serializable, Map<String, Object>> originalRowsMap, LocalDateTime updatedTime) {
        return Collections.emptyList();
    }

    /**
     * Process the OneToMany and ManyToMany fields associated with the current model,
     * without performing association permission check.
     */
    public boolean processXToManyData(List<Map<String, Object>> rows) {
        return false;
    }

}
