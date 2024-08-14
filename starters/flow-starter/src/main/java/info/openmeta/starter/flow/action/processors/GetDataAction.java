package info.openmeta.starter.flow.action.processors;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.GetDataParams;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import info.openmeta.starter.flow.utils.FlowUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static info.openmeta.framework.base.constant.BaseConstant.MAX_BATCH_SIZE;
import static info.openmeta.starter.flow.action.enums.ActionGetDataType.*;

/**
 * Processor for GetData action.
 * Get data based on the specified model, filters, fields, and orders.
 * Supports getting single row, multiple rows, single field value, list of single field values, existence, and count.
 * The amount of data retrieved does not exceed MAX_BATCH_SIZE.
 * If it is within a LoopByPage node, when GetDataParams is not set limitSize,
 * it is preferred to get data according to the paging parameters of the loop node.
 */
@Component
public class GetDataAction implements ActionProcessor<GetDataParams> {

    @Autowired
    private ModelService<? extends Serializable> modelService;

    @Override
    public FlowActionType getActionType() {
        return FlowActionType.GET_DATA;
    }

    @Override
    public Class<GetDataParams> getParamsClass() {
        return GetDataParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, GetDataParams actionParams) {
        Assert.notBlank(actionParams.getModel(),
                "The model name parameter for GetDataAction {0} cannot be blank.", flowAction.getName());
        Assert.notNull(actionParams.getGetDataType(),
                "The data type parameter for GetDataAction {0} cannot be blank.", flowAction.getName());
        if (ONE_FIELD_VALUE.equals(actionParams.getGetDataType())) {
            Assert.isTrue(actionParams.getFields() != null && actionParams.getFields().size() == 1,
                    "When the GetDataAction {0} is configured to extract a specified field, " +
                            "the fields parameter can only specify one field name.", flowAction.getName());
        }
        if (actionParams.getLimitSize() != null) {
            Assert.isTrue(actionParams.getLimitSize() > 0 && actionParams.getLimitSize() <= MAX_BATCH_SIZE,
                    "When the GetDataAction action {0}  is configured to retrieve multiple rows, " +
                            "the data size parameter must be greater than 0 and less than or equal to " +
                            "the maximum batch limit {1}.", flowAction.getName(), MAX_BATCH_SIZE);
        }
    }

    /**
     * Execute the GetDataAction processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     * @param actionContext The action context
     */
    @Override
    public void execute(FlowAction flowAction, GetDataParams actionParams, ActionContext actionContext) {
        Filters clonedFilters = null;
        if (!Filters.isEmpty(actionParams.getFilters())) {
            // Resolve the variables and calculation formulas in the filters
            clonedFilters = actionParams.getFilters().deepCopy();
            FlowUtils.resolveFilterValue(actionParams.getModel(), clonedFilters, actionContext);
        }
        Object result = null;
        if (COUNT_TYPES.contains(actionParams.getGetDataType())) {
            result = this.executeCount(actionParams, clonedFilters);
        } else if (SINGLE_ROW_TYPES.contains(actionParams.getGetDataType())) {
            result = this.executeSingleRow(actionParams, clonedFilters);
        } else if (MULTI_ROW_TYPES.contains(actionParams.getGetDataType())) {
            result = this.executeMultiRows(actionParams, clonedFilters);
        }
        // Put the result into the actionContext
        actionContext.put(flowAction.getCode(), result);
    }

    /**
     * Execute count or determine whether it exists
     */
    private Object executeCount(GetDataParams actionParams, Filters filters) {
        Long count = modelService.count(actionParams.getModel(), filters);
        if (COUNT.equals(actionParams.getGetDataType())) {
            return count;
        } else if (EXIST.equals(actionParams.getGetDataType())) {
            // Extract the count value, convert it to a boolean type, and determine whether it exists
            return count > 0;
        }
        return null;
    }

    /**
     * Get single row data and extract the specified field value according to the configuration
     */
    private Object executeSingleRow(GetDataParams actionParams, Filters filters) {
        FlexQuery flexQuery = new FlexQuery(actionParams.getFields(), filters, actionParams.getOrders());
        // Set limitSize to 1 when getting a single row of data
        flexQuery.setLimitSize(1);
        Map<String, Object> row = modelService.searchOne(actionParams.getModel(), flexQuery);
        if (ONE_FIELD_VALUE.equals(actionParams.getGetDataType())) {
            // Extract the specified single field value of the first row data and update it to actionContext
            String fieldName = actionParams.getFields().getFirst();
            return row.get(fieldName);
        } else {
            return row;
        }
    }

    /**
     * Get multiple rows of data and extract the value list of the specified field according to the configuration
     */
    private Object executeMultiRows(GetDataParams actionParams, Filters filters) {
        FlexQuery flexQuery = new FlexQuery(actionParams.getFields(), filters, actionParams.getOrders());
        // Set limitSize when getting non-paged multiple rows of data, not exceeding the maximum allowed quantity
        if (actionParams.getLimitSize() != null) {
            flexQuery.setLimitSize(actionParams.getLimitSize());
        } else {
            flexQuery.setLimitSize(MAX_BATCH_SIZE);
        }
        List<Map<String, Object>> rows = modelService.searchList(actionParams.getModel(), flexQuery);
        if (!rows.isEmpty() && ONE_FIELD_VALUES.equals(actionParams.getGetDataType())) {
            // Extract the value list of the specified field from multiple rows of data,
            // and the field value is extracted to Set, which is automatically deduplicated
            return rows.stream().map(row -> row.get(actionParams.getFields().getFirst())).collect(Collectors.toSet());
        } else {
            // Multiple rows of data
            return rows;
        }
    }
}
