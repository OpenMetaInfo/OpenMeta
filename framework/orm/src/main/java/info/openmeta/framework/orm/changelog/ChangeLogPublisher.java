package info.openmeta.framework.orm.changelog;

import info.openmeta.framework.base.constant.TimeConstant;
import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.orm.changelog.event.TransactionEvent;
import info.openmeta.framework.orm.changelog.message.dto.ChangeLog;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.utils.ListUtils;
import jakarta.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * The publisher of ChangeLog, publish ChangeLog to local thread variable and publish ChangeLog event
 * <p>
 * TODO: Add a configuration item, such as enableChangeLog, to suit for applications of different scales.
 *  1. Small and medium scale applications: enableChangeLog = true, use ChangeLogPublisher to publish ChangeLog.
 *  2. High concurrency large-scale applications: Selective use Debezium to capture and publish database changes.
 *
 */
@Service
public class ChangeLogPublisher {

    private static final TransactionEvent CHANGE_LOG_EVENT = new TransactionEvent();

    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;

    /**
     * Save changeLogs to thread variable and publish ChangeLog event (only publish once in a transaction).
     * If the thread variable changeLogs is not empty, add the ChangeLog resource to the thread variable.
     * @param changeLogs ChangeLog list
     */
    private void publish(List<ChangeLog> changeLogs) {
        if (changeLogs == null || changeLogs.isEmpty()) {
            return;
        }
        if (ChangeLogHolder.isEmpty()) {
            ChangeLogHolder.set(changeLogs);
            applicationEventPublisher.publishEvent(CHANGE_LOG_EVENT);
        } else {
            ChangeLogHolder.add(changeLogs);
        }
    }

    /**
     * Publish creation changeLog
     * @param model model name
     * @param createdRows created data list
     * @param createdTime created time
     */
    public void publishCreationLog(String model, List<Map<String, Object>> createdRows, LocalDateTime createdTime) {
        String primaryKey = ModelManager.getModelPrimaryKey(model);
        List<ChangeLog> changeLogs = createdRows.stream().map(row -> {
            Serializable pKey = (Serializable) row.get(primaryKey);
            ChangeLog changeLog = generateChangeLog(model, AccessType.CREATE, pKey, createdTime);
            changeLog.setDataAfterChange(row);
            return changeLog;
        }).collect(Collectors.toList());
        this.publish(changeLogs);
    }

    /**
     * Publish update changeLog, add the original data to dataBeforeChange
     * @param model model name
     * @param updatedRows changed data list
     * @param originalRowsMap original data list, with id as key and single data row as value
     * @param updatedTime updated time
     */
    public void publishUpdateLog(String model, List<Map<String, Object>> updatedRows, Map<Serializable, Map<String, Object>> originalRowsMap, LocalDateTime updatedTime) {
        String primaryKey = ModelManager.getModelPrimaryKey(model);
        // Deep copy to avoid affecting the original data when removing the primary key
        updatedRows = ListUtils.deepCopy(updatedRows);
        List<ChangeLog> changeLogs = updatedRows.stream().map(row -> {
            Serializable pKey = (Serializable) row.get(primaryKey);
            // Remove primary key and audit fields from the change data
            row.remove(primaryKey);
            row.remove(ModelConstant.ID);
            ModelConstant.AUDIT_UPDATE_FIELDS.forEach(row::remove);
            ChangeLog changeLog = generateChangeLog(model, AccessType.UPDATE, pKey, updatedTime);
            changeLog.setDataBeforeChange(originalRowsMap.get(pKey));
            changeLog.setDataAfterChange(row);
            return changeLog;
        }).collect(Collectors.toList());
        this.publish(changeLogs);
    }

    /**
     * Publish deletion changeLog, add the original data to dataBeforeChange
     * @param model model name
     * @param deletedRows deleted data list
     * @param deleteTime deleted time
     */
    public void publishDeletionLog(String model, List<Map<String, Object>> deletedRows, LocalDateTime deleteTime) {
        String primaryKey = ModelManager.getModelPrimaryKey(model);
        List<ChangeLog> changeLogs = deletedRows.stream().map(row -> {
            Serializable pKey = (Serializable) row.get(primaryKey);
            ChangeLog changeLog = generateChangeLog(model, AccessType.DELETE, pKey, deleteTime);
            changeLog.setDataBeforeChange(row);
            return changeLog;
        }).collect(Collectors.toList());
        this.publish(changeLogs);
    }

    /**
     * Generate a ChangeLog object
     * @param model model name
     * @param accessType access type
     * @param id id of the data row
     */
    private ChangeLog generateChangeLog(String model, AccessType accessType, Serializable id, @NotNull LocalDateTime updatedTime) {
        Context context = ContextHolder.getContext();
        ChangeLog changeLog = new ChangeLog();
        changeLog.setRequestId(context.getRequestId());
        changeLog.setModel(model);
        changeLog.setRowId((Long) id);
        changeLog.setAccessType(accessType);
        // Set changeLog audit fields
        changeLog.setChangedId(context.getUserId());
        changeLog.setChangedBy(context.getName());
        changeLog.setChangedTime(updatedTime.format(TimeConstant.DATETIME_FORMATTER));
        return changeLog;
    }

}