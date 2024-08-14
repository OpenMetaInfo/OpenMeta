package info.openmeta.framework.orm.jdbc.pipeline.processor;

import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.UUIDUtils;
import info.openmeta.framework.orm.enums.IdStrategy;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.utils.IdUtils;
import info.openmeta.framework.orm.utils.ObjectId;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Supplier;

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
        switch (idStrategy) {
            case DB_AUTO_ID:
                // Skip auto-increment ID
                return;
            case SNOWFLAKE_ID:
//                generateIds(rows, IdUtils::snowflakeId);
                break;
            case OBJECT_ID:
                generateIds(rows, () -> ObjectId.get().toString());
                break;
            case SHORT_UUID:
                generateIds(rows, UUIDUtils::short22UUID);
                break;
            case UUID:
                generateIds(rows, () -> UUID.randomUUID().toString());
                break;
            case EXTERNAL_ID:
                formatExternalIds(rows);
                break;
            default:
                throw new IllegalArgumentException("Unknown ID strategy: " + idStrategy);
        }
    }

    /**
     * Generate ID for each row
     *
     * @param rows List of data to be created
     * @param idGenerator ID generator
     */
    private void generateIds(Collection<Map<String, Object>> rows, Supplier<String> idGenerator) {
        rows.forEach(row -> row.put(fieldName, idGenerator.get()));
    }

    /**
     * Format external ID
     *
     * @param rows List of data to be created
     */
    private void formatExternalIds(Collection<Map<String, Object>> rows) {
        rows.forEach(row -> {
            Object idValue = row.get(fieldName);
            Assert.notNull(idValue, "Model {0} external ID cannot be empty!", modelName);
            row.put(fieldName, IdUtils.formatId(idValue, metaField.getFieldType()));
        });
    }

    @Override
    public void batchProcessInputRows(List<Map<String, Object>> rows, AccessType accessType) {
        if (AccessType.CREATE.equals(accessType)) {
            this.processIdByIdStrategy(rows);
        }
    }
}
