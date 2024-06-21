package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.UUIDUtils;
import info.openmeta.framework.orm.enums.IdStrategy;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.utils.IdUtils;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * ID field processor
 */
public class IdProcessor extends BaseProcessor {

    public IdProcessor(MetaField metaField) {
        super(metaField);
    }

    /**
     * Generate or process primary key based on idStrategy when creating data.
     *
     * @param rows List of data to be created
     */
    private void processIdByIdStrategy(Collection<Map<String, Object>> rows) {
        IdStrategy idStrategy = ModelManager.getIdStrategy(modelName);
        if (IdStrategy.SHORT_UUID.equals(idStrategy)) {
            // Generate short UUID
            rows.forEach(row -> row.put(fieldName, UUIDUtils.short22UUID()));
        } else if (IdStrategy.UUID.equals(idStrategy)) {
            // Normal UUID
            rows.forEach(row -> row.put(fieldName, UUID.randomUUID().toString()));
        } else if (IdStrategy.EXTERNAL_ID.equals(idStrategy)) {
            // Format external ID
            rows.forEach(row -> {
                Assert.notNull(row.get(fieldName), "Model {0} external ID cannot be empty!", modelName);
                row.put(fieldName, IdUtils.formatId(row.get(fieldName), metaField.getFieldType()));
            });
        }
    }

    @Override
    public void batchProcessInputRows(List<Map<String, Object>> rows, AccessType accessType) {
        if (AccessType.CREATE.equals(accessType)) {
            this.processIdByIdStrategy(rows);
        }
    }
}
