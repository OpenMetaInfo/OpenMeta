package info.openmeta.starter.flow.node.processors;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.node.NodeProcessor;
import info.openmeta.starter.flow.node.params.GetDataParams;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.FlowNodeType;
import info.openmeta.starter.flow.utils.FlowUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static info.openmeta.framework.base.constant.BaseConstant.MAX_BATCH_SIZE;
import static info.openmeta.starter.flow.node.enums.NodeGetDataType.*;

/**
 * Processor for GetData node.
 * Get data based on the specified model, filters, fields, and orders.
 * Supports getting single row, multiple rows, single field value, list of single field values, existence, and count.
 * The amount of data retrieved does not exceed MAX_BATCH_SIZE.
 * If it is within a LoopByPage node, when GetDataParams is not set limitSize,
 * it is preferred to get data according to the paging parameters of the loop node.
 */
@Component
public class GetDataNode implements NodeProcessor<GetDataParams> {

    @Autowired
    private ModelService<? extends Serializable> modelService;

    @Override
    public FlowNodeType getNodeType() {
        return FlowNodeType.GET_DATA;
    }

    @Override
    public Class<GetDataParams> getParamsClass() {
        return GetDataParams.class;
    }

    /**
     * Validate the parameters of the specified FlowNode under the current node processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     */
    @Override
    public void validateParams(FlowNode flowNode, GetDataParams nodeParams) {
        Assert.notBlank(nodeParams.getModelName(),
                "The model name parameter for GetDataNode {0} cannot be blank.", flowNode.getName());
        Assert.notNull(nodeParams.getGetDataType(),
                "The data type parameter for GetDataNode {0} cannot be blank.", flowNode.getName());
        if (ONE_FIELD_VALUE.equals(nodeParams.getGetDataType())) {
            Assert.isTrue(nodeParams.getFields() != null && nodeParams.getFields().size() == 1,
                    "When the GetDataNode {0} is configured to extract a specified field, " +
                            "the fields parameter can only specify one field name.", flowNode.getName());
        }
        if (nodeParams.getLimitSize() != null) {
            Assert.isTrue(nodeParams.getLimitSize() > 0 && nodeParams.getLimitSize() <= MAX_BATCH_SIZE,
                    "When the GetDataNode node {0}  is configured to retrieve multiple rows, " +
                            "the data size parameter must be greater than 0 and less than or equal to " +
                            "the maximum batch limit {1}.", flowNode.getName(), MAX_BATCH_SIZE);
        }
    }

    /**
     * Execute the GetDataNode processor.
     *
     * @param flowNode The flow node
     * @param nodeParams The parameters of the flow node.
     * @param nodeContext The node context
     */
    @Override
    public void execute(FlowNode flowNode, GetDataParams nodeParams, NodeContext nodeContext) {
        Filters clonedFilters = null;
        if (!Filters.isEmpty(nodeParams.getFilters())) {
            // Resolve the variables and calculation formulas in the filters
            clonedFilters = nodeParams.getFilters().deepCopy();
            FlowUtils.resolveFilterValue(nodeParams.getModelName(), clonedFilters, nodeContext);
        }
        Object result = null;
        if (COUNT_TYPES.contains(nodeParams.getGetDataType())) {
            result = this.executeCount(nodeParams, clonedFilters);
        } else if (SINGLE_ROW_TYPES.contains(nodeParams.getGetDataType())) {
            result = this.executeSingleRow(nodeParams, clonedFilters);
        } else if (MULTI_ROW_TYPES.contains(nodeParams.getGetDataType())) {
            result = this.executeMultiRows(nodeParams, clonedFilters);
        }
        // Put the result into the nodeContext
        nodeContext.put(flowNode.getId(), result);
    }

    /**
     * Execute count or determine whether it exists
     */
    private Object executeCount(GetDataParams nodeParams, Filters filters) {
        Long count = modelService.count(nodeParams.getModelName(), filters);
        if (COUNT.equals(nodeParams.getGetDataType())) {
            return count;
        } else if (EXIST.equals(nodeParams.getGetDataType())) {
            // Extract the count value, convert it to a boolean type, and determine whether it exists
            return count > 0;
        }
        return null;
    }

    /**
     * Get single row data and extract the specified field value according to the configuration
     */
    private Object executeSingleRow(GetDataParams nodeParams, Filters filters) {
        FlexQuery flexQuery = new FlexQuery(nodeParams.getFields(), filters, nodeParams.getOrders());
        // Set limitSize to 1 when getting a single row of data
        flexQuery.setLimitSize(1);
        Map<String, Object> row = modelService.searchOne(nodeParams.getModelName(), flexQuery);
        if (ONE_FIELD_VALUE.equals(nodeParams.getGetDataType())) {
            // Extract the specified single field value of the first row data and update it to actionContext
            String fieldName = nodeParams.getFields().getFirst();
            return row.get(fieldName);
        } else {
            return row;
        }
    }

    /**
     * Get multiple rows of data and extract the value list of the specified field according to the configuration
     */
    private Object executeMultiRows(GetDataParams nodeParams, Filters filters) {
        FlexQuery flexQuery = new FlexQuery(nodeParams.getFields(), filters, nodeParams.getOrders());
        // Set limitSize when getting non-paged multiple rows of data, not exceeding the maximum allowed quantity
        if (nodeParams.getLimitSize() != null) {
            flexQuery.setLimitSize(nodeParams.getLimitSize());
        } else {
            flexQuery.setLimitSize(MAX_BATCH_SIZE);
        }
        List<Map<String, Object>> rows = modelService.searchList(nodeParams.getModelName(), flexQuery);
        if (!rows.isEmpty() && ONE_FIELD_VALUES.equals(nodeParams.getGetDataType())) {
            // Extract the value list of the specified field from multiple rows of data,
            // and the field value is extracted to Set, which is automatically deduplicated
            return rows.stream().map(row -> row.get(nodeParams.getFields().getFirst())).collect(Collectors.toSet());
        } else {
            // Multiple rows of data
            return rows;
        }
    }
}
