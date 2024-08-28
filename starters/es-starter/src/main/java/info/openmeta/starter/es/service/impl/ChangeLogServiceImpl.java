package info.openmeta.starter.es.service.impl;

import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.changelog.message.dto.ChangeLog;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.jdbc.pipeline.DataPipelineProxy;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.orm.service.PermissionService;
import info.openmeta.starter.es.service.ChangeLogService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.util.*;

import static info.openmeta.framework.base.enums.AccessType.*;
import static info.openmeta.framework.orm.constant.ModelConstant.SLICE_ID;

/**
 * ChangeLog Service Implementation
 */
@Service
public class ChangeLogServiceImpl extends ESServiceImpl<ChangeLog> implements ChangeLogService {

    @Lazy
    @Autowired
    private PermissionService permissionService;

    @Lazy
    @Autowired
    private ModelService<Serializable> modelService;

    @Autowired
    private DataPipelineProxy dataPipelineProxy;

    @Value("${spring.elasticsearch.index.changelog}")
    private String changeLogIndexName;

    /** ES service implementation class must specify the index name by implementing this method */
    public String getIndexName() {
        return changeLogIndexName;
    }

    /**
     * Get the change log by the id of the business data model.
     *
     * @param modelName model name
     * @param id primary key id
     * @param order sort rule based on change time, default is reverse order, only support DESC, ASC string
     * @param includeCreation whether to include data at creation time, default is false, that is, not included
     * @return a page of change log list
     */
    public Page<ChangeLog> getChangeLog(String modelName, Serializable id, Page<ChangeLog> page, String order, boolean includeCreation) {
        // Check if current user has access to the model and id
        permissionService.checkIdsFieldsAccess(modelName, Collections.singletonList(id), null, READ);
        page = this.getRowChangeLog(modelName, id, page, order, includeCreation);
        return this.processChangeLogData(modelName, page);
    }

    /**
     * Get the change log by the primary key of the timeline model.
     *
     * @param modelName model name
     * @param sliceId primary key of the timeline model
     * @param page page object
     * @param order sort rule based on change time, default is reverse order, only support DESC, ASC string
     * @param includeCreation whether to include data at creation time, default is false, that is, not included
     * @return a page of change log list
     */
    public Page<ChangeLog> getSliceChangeLog(String modelName, Serializable sliceId, Page<ChangeLog> page, String order, boolean includeCreation) {
        // Get the business ids of the timeline model
        List<Serializable> ids = modelService.getIds(modelName, Filters.of(SLICE_ID, Operator.EQUAL, sliceId));
        Assert.notEmpty(ids,
                "Timeline model {0} does not exist slice sliceId={1} data!", modelName, sliceId);
        // Check if current user has access to the timeline model and business id
        permissionService.checkIdsFieldsAccess(modelName, ids, null, READ);
        this.getRowChangeLog(modelName, sliceId, page, order, includeCreation);
        return this.processChangeLogData(modelName, page);
    }

    /**
     * Get the ChangeLog page with the specified query conditions
     *
     * @param model     model name
     * @param filters   filter conditions
     * @param orders    sort conditions
     * @param page      page object
     * @param trackTotal whether to count the total when the total number is greater than 10000
     * @return a page of list
     */
    public Page<ChangeLog> searchPageByModel(String model, Filters filters, Orders orders, Page<ChangeLog> page, boolean trackTotal) {
        // TODO: Check if current user has access to the model, and append filters of the permission conditions
        this.searchPage(filters, orders, page, trackTotal);
        this.processChangeLogData(model, page);
        return page;
    }

    /**
     * Get the ChangeLog list of the specified primary key.
     *
     * @param model     model name
     * @param pKey      primary key, corresponding to the sliceId of the timeline record
     * @param page      page object
     * @param order     sort rule, default is reverse order by change time, only support DESC, ASC string
     * @param includeCreation whether to include data at creation time, default is false, that is, not included
     * @return a page of ChangeLog list
     */
    public Page<ChangeLog> getRowChangeLog(String model, Serializable pKey, Page<ChangeLog> page, String order, boolean includeCreation) {
        // Default sort by changedTime in reverse order
        Orders orders = Orders.DESC.equals(order) ? Orders.ofDesc(ChangeLog::getChangedTime) : Orders.ofAsc(ChangeLog::getChangedTime);
        // Query the change log of the specified filters and rowId
        Filters filters = Filters.eq(ChangeLog::getModel, model).andEq(ChangeLog::getRowId, pKey);
        if (!includeCreation) {
            // When not including the initial creation record, only match UPDATE and DELETE records
            filters.andIn(ChangeLog::getAccessType, Arrays.asList(UPDATE, DELETE));
        }
        return this.searchPage(filters, orders, page, false);
    }

    /**
     * Read the data change records according to the id of the business data model.
     *
     * @param modelName model name
     * @param page page object
     * @return a page of change log list
     */
    private Page<ChangeLog> processChangeLogData(String modelName, Page<ChangeLog> page) {
        List<Map<String, Object>> changeLogDataList = new ArrayList<>();
        Set<String> fields = new HashSet<>();
        page.getRows().forEach(changeLog -> {
            if (UPDATE.equals(changeLog.getAccessType())) {
                // UPDATE contains data before and after change
                fields.addAll(changeLog.getDataBeforeChange().keySet());
                changeLogDataList.add(changeLog.getDataBeforeChange());
                fields.addAll(changeLog.getDataAfterChange().keySet());
                changeLogDataList.add(changeLog.getDataAfterChange());
            } else if (CREATE.equals(changeLog.getAccessType())) {
                // CREATE only has data after change
                fields.addAll(changeLog.getDataAfterChange().keySet());
                changeLogDataList.add(changeLog.getDataAfterChange());
            }
        });
        // Enhance the field values in before and after data, only retain the fields existing in the metadata.
        // TODO: Exclude fields that are not accessible
        fields.retainAll(ModelManager.getModelStoredFields(modelName));
        FlexQuery flexQuery = new FlexQuery(fields);
        flexQuery.setConvertType(ConvertType.KEY_AND_DISPLAY);
        dataPipelineProxy.processReadData(modelName, flexQuery, changeLogDataList);
        return page;
    }

}