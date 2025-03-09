package info.openmeta.starter.designer.version.impl;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.constant.TimeConstant;
import info.openmeta.framework.base.enums.AccessType;
import info.openmeta.framework.base.utils.DateUtils;
import info.openmeta.framework.orm.changelog.message.dto.ChangeLog;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.jdbc.pipeline.DataPipelineProxy;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.starter.designer.constant.VersionConstant;
import info.openmeta.starter.designer.dto.ModelChangesDTO;
import info.openmeta.starter.designer.dto.RowChangeDTO;
import info.openmeta.starter.designer.entity.DesignAppEnv;
import info.openmeta.starter.designer.version.VersionControl;
import info.openmeta.starter.es.service.ChangeLogService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static info.openmeta.framework.base.enums.AccessType.*;

/**
 * Version control implementation
 */
@Component
public class VersionControlImpl implements VersionControl {

    @Autowired
    private ChangeLogService changeLogService;

    @Autowired
    private ModelService<Long> modelService;

    @Autowired
    private DataPipelineProxy dataPipelineProxy;

    /**
     * Get the change data of the model, including created, updated, and deleted data.
     *
     * @param appEnv         app environment
     * @param versionedModel Version controlled design model name
     * @param startTime      Start time of the change
     * @return ModelChangesDTO
     */
    @Override
    public ModelChangesDTO getModelChanges(DesignAppEnv appEnv, String versionedModel, LocalDateTime startTime) {
        ModelChangesDTO modelChangesDTO = new ModelChangesDTO(versionedModel);
        boolean isSoftDeleted = ModelManager.isSoftDeleted(versionedModel);
        String softDeleteField = ModelManager.getSoftDeleteField(versionedModel);
        // Read unpublished ids of the current application and model, including deleted unpublished data
        List<Map<String, Object>> changedData = this.getChangedDataFromDB(appEnv, versionedModel, startTime);
        Map<Serializable, Map<String, Object>> updatedDataMap = new HashMap<>();
        for (Map<String, Object> row : changedData) {
            if (startTime == null || startTime.isBefore((LocalDateTime) row.get(ModelConstant.CREATED_TIME))) {
                // Created data before the first publish, ignore deleted data before the first publish
                if (isSoftDeleted && !Boolean.TRUE.equals(row.get(softDeleteField))) {
                    modelChangesDTO.addCreatedRow(convertToRowChangeDTO(versionedModel, AccessType.CREATE, row));
                }
            } else if (isSoftDeleted && Boolean.TRUE.equals(row.get(softDeleteField))) {
                // Deleted data after published
                modelChangesDTO.addDeletedRow(convertToRowChangeDTO(versionedModel, AccessType.DELETE, row));
            } else {
                // Extract the updated data from the changelog
                updatedDataMap.put((Serializable) row.get(ModelConstant.ID), row);
            }
        }
        if (!updatedDataMap.isEmpty()) {
            // Get changelogs sorted in ascending order
            List<ChangeLog> updatedChangeLogs = this.getChangeLogsOfData(versionedModel, updatedDataMap.keySet(), startTime);
            // Group changelogs by rowId
            Map<Serializable, List<ChangeLog>> updatedChangeLogsMap = updatedChangeLogs.stream()
                    .collect(Collectors.groupingBy(ChangeLog::getRowId));
            updatedChangeLogsMap.forEach((rowId, changeLogs) -> {
                RowChangeDTO rowChangeDTO = mergeUpdatedToRowChangeDTO(changeLogs, updatedDataMap.get(rowId));
                modelChangesDTO.addUpdatedRow(rowChangeDTO);
            });
        }
        if (modelChangesDTO.getCreatedRows().isEmpty() && modelChangesDTO.getUpdatedRows().isEmpty()
                && modelChangesDTO.getDeletedRows().isEmpty()) {
            return null;
        }
        return modelChangesDTO;
    }


    /**
     * Get changed data of the specified App, Env and Model, after the specified time.
     * Changed data includes the deleted data before the first publish.
     *
     * @param appEnv App environment
     * @param versionedModel Version controlled design model name
     * @param startTime Start time of the change
     * @return Changed data list
     */
    private List<Map<String, Object>> getChangedDataFromDB(DesignAppEnv appEnv, String versionedModel, LocalDateTime startTime) {
        // Read unpublished data of the current app, env and model from the database
        Filters changedDataFilters = new Filters().eq(VersionConstant.APP_ID, appEnv.getAppId())
                .eq(VersionConstant.ENV_ID, appEnv.getId());
        if (ModelManager.isSoftDeleted(versionedModel)) {
            String softDeleteField = ModelManager.getSoftDeleteField(versionedModel);
            changedDataFilters.in(softDeleteField, Arrays.asList(false, true, null));
        }
        if (startTime != null) {
            changedDataFilters.ge(ModelConstant.UPDATED_TIME, startTime);
        }
        // Read current data, excluding custom bound OneToMany, ManyToMany field data
        Set<String> fields = ModelManager.getModelFieldsWithoutXToMany(versionedModel);
        FlexQuery flexQuery = new FlexQuery(fields, changedDataFilters);
        List<Map<String, Object>> changedData = modelService.searchList(versionedModel, flexQuery);
        // Remove unpublished data that has been deleted before the first publish
        boolean isSoftDeleted = ModelManager.isSoftDeleted(versionedModel);
        String softDeleteField = ModelManager.getSoftDeleteField(versionedModel);
        changedData.removeIf(row -> {
            if (isSoftDeleted && Boolean.TRUE.equals(row.get(softDeleteField))) {
                if (startTime == null) {
                    return true;
                } else return ((LocalDateTime) row.get(ModelConstant.CREATED_TIME)).isAfter(startTime);
            }
            return false;
        });
        return changedData;
    }

    /**
     * Get the changeLogs of the specified data ids, after the specified time.
     *
     * @param versionedModel Version controlled design model name
     * @param rowIds List of changed data ids
     * @param startTime Start time of the change
     * @return List of changeLogs
     */
    private List<ChangeLog> getChangeLogsOfData(String versionedModel, Collection<Serializable> rowIds, LocalDateTime startTime) {
        // Query unpublished data change records, sorted by changedTime in descending order
        Filters filters = new Filters().eq(ChangeLog::getModel, versionedModel)
                .in(ChangeLog::getRowId, rowIds);
        if (startTime != null) {
            filters.ge(ChangeLog::getChangedTime, startTime.format(TimeConstant.DATETIME_FORMATTER));
        }
        Orders orders = Orders.ofAsc(ChangeLog::getChangedTime);
        FlexQuery flexQuery = new FlexQuery(filters, orders);
        // Execute query
        List<ChangeLog> changeLogs = new ArrayList<>();
        Page<ChangeLog> page = Page.of(BaseConstant.DEFAULT_PAGE_NUMBER, BaseConstant.DEFAULT_BATCH_SIZE);
        do {
            changeLogService.searchPageByModel(versionedModel, flexQuery, page, true);
            changeLogs.addAll(page.getRows());
        } while (page.toNext());
        return changeLogs;
    }

    /**
     * Convert the changed data to RowChangeDTO
     *
     * @param modelName Model name
     * @param accessType Access type
     * @param row Changed data
     * @return RowChangeDTO object
     */
    private static RowChangeDTO convertToRowChangeDTO(String modelName, AccessType accessType, Map<String, Object> row) {
        RowChangeDTO rowChangeDTO = new RowChangeDTO(modelName, (Serializable) row.get(ModelConstant.ID));
        rowChangeDTO.setAccessType(accessType);
        rowChangeDTO.setCurrentData(row);
        rowChangeDTO.setLastChangedId((Long) row.get(ModelConstant.UPDATED_ID));
        rowChangeDTO.setLastChangedBy((String) row.get(ModelConstant.UPDATED_BY));
        rowChangeDTO.setLastChangedTime(DateUtils.dateTimeToString(row.get(ModelConstant.UPDATED_TIME)));
        return rowChangeDTO;
    }

    /**
     * Merge List<ChangeLog> with the same id into one RowChangeDTO
     *
     * @param changeLogs List of change records with the same id, sorted in ascending order
     * @param currentData Current data
     * @return RowChangeDTO object
     */
    private static RowChangeDTO mergeUpdatedToRowChangeDTO(List<ChangeLog> changeLogs, Map<String, Object> currentData) {
        ChangeLog lastLog = changeLogs.getLast();
        RowChangeDTO rowChangeDTO = new RowChangeDTO(lastLog.getModel(), lastLog.getRowId());
        rowChangeDTO.setAccessType(UPDATE);
        rowChangeDTO.setCurrentData(currentData);
        rowChangeDTO.setLastChangedId(lastLog.getChangedId());
        rowChangeDTO.setLastChangedBy(lastLog.getChangedBy());
        rowChangeDTO.setLastChangedTime(lastLog.getChangedTime());
        // Merge data before the change in DESC order
        for (int i = changeLogs.size() - 1; i >= 0; i--) {
            ChangeLog changeLog = changeLogs.get(i);
            rowChangeDTO.mergeDataBeforeChange(changeLog.getDataBeforeChange());
        }
        // Merge data after the change in ASC order
        for (ChangeLog changeLog : changeLogs) {
            rowChangeDTO.mergeDataAfterChange(changeLog.getDataAfterChange());
        }
        return rowChangeDTO;
    }

    /**
     * Format the Map structure data before and after the change in the Update scenario
     * @param versionedModel Version controlled design model name
     * @param rowChangeDTOList List of RowChangeDTO
     */
    private void formatRowChangeDTOList(String versionedModel, List<RowChangeDTO> rowChangeDTOList) {
        List<Map<String, Object>> updateDataList = new ArrayList<>();
        Set<String> fields = ModelManager.getModelUpdatableFields(versionedModel);
        rowChangeDTOList.forEach(rowChangeDTO -> {
            if (UPDATE.equals(rowChangeDTO.getAccessType())) {
                // UPDATE contains data before and after the change
                updateDataList.add(rowChangeDTO.getDataBeforeChange());
                updateDataList.add(rowChangeDTO.getDataAfterChange());
            }
        });
        // Format the field values before and after the change for Update data
        FlexQuery flexQuery = new FlexQuery(fields);
        dataPipelineProxy.processReadData(versionedModel, flexQuery, updateDataList);
    }

}