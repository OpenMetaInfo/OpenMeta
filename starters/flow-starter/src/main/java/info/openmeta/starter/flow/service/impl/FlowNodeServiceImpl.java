package info.openmeta.starter.flow.service.impl;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.exception.JSONException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.FlowNodeType;
import info.openmeta.starter.flow.enums.NodeExceptionSignal;
import info.openmeta.starter.flow.node.NodeContext;
import info.openmeta.starter.flow.node.NodeFactory;
import info.openmeta.starter.flow.node.params.LoopByDatasetParams;
import info.openmeta.starter.flow.node.params.LoopByPageParams;
import info.openmeta.starter.flow.service.FlowNodeService;
import info.openmeta.starter.flow.utils.FlowUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StopWatch;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import static info.openmeta.framework.base.constant.BaseConstant.DEFAULT_PAGE_SIZE;
import static info.openmeta.framework.base.constant.BaseConstant.MAX_BATCH_SIZE;

/**
 * FlowNode Model Service Implementation
 */
@Slf4j
@Service
public class FlowNodeServiceImpl extends EntityServiceImpl<FlowNode, Long> implements FlowNodeService {

    @Autowired
    private NodeFactory<?> nodeFactory;

    /**
     * Verify whether the condition expression is met, and then determine whether to execute the current node or node.
     * The Flow Engine has set the env to be unmodifiable to avoid data pollution
     *
     * @param condition condition expression
     * @param nodeContext flow environment variables
     * @return whether to execute
     */
    private boolean isValidNodeCondition(String condition, NodeContext nodeContext) {
        if (StringUtils.isNotBlank(condition)) {
            return ComputeUtils.executeBoolean(condition, nodeContext.getEnv());
        }
        // Execute directly when the execution condition is empty.
        return true;
    }

    /**
     * Verify and extract the loop parameters of the `LoopByDataset` node,
     * where the dataset parameter must be a variable that exists in the context.
     *
     * @param flowNode flow node
     * @return loop parameters
     */
    private LoopByDatasetParams extractLoopByDatasetParams(FlowNode flowNode) {
        // Deserialize the dataset loop parameters into a `LoopByDatasetParams` object.
        LoopByDatasetParams loopByDatasetParams;
        try {
            loopByDatasetParams = JsonMapper.jsonNodeToObject(flowNode.getNodeParams(), LoopByDatasetParams.class);
        } catch (JSONException e) {
            throw new JSONException("Failed to parse the loop parameters of the `LoopByDataset` node {0}: {1}",
                    flowNode.getName(), e.getMessage());
        }
        Assert.notNull(loopByDatasetParams,
                "Loop parameters are not defined for the `LoopByDataset` node {0}!", flowNode.getName());
        Assert.notBlank(loopByDatasetParams.getDataSetParam(),
                "The `LoopByDataset` node {0} must specify a dataset variable!", flowNode.getName());
        Assert.isTrue(StringTools.isVariable(loopByDatasetParams.getDataSetParam()),
                "The dataset variable {0} of the `LoopByDataset` node {1} must be identified with `#{}`!",
                loopByDatasetParams.getDataSetParam(), flowNode.getName());
        Assert.notBlank(loopByDatasetParams.getLoopItemNaming(),
                "The loop parameter name of the `LoopByDataset` node {0} cannot be empty!",
                loopByDatasetParams.getLoopItemNaming());
        return loopByDatasetParams;
    }

    /**
     * Verify and extract the loop parameters of the paginated loop node.
     *
     * @param flowNode flow node
     * @return loop parameters
     */
    private LoopByPageParams extractLoopByPageParams(FlowNode flowNode) {
        // Deserialize the paginated loop parameters into a `LoopByPageParams` object.
        LoopByPageParams loopByPageParams;
        try {
            loopByPageParams = JsonMapper.jsonNodeToObject(flowNode.getNodeParams(), LoopByPageParams.class);
        } catch (JSONException e) {
            throw new JSONException("Failed to parse the pagination parameters of the `LoopByPage` node {0}: {1}",
                    flowNode.getName(), e.getMessage());
        }
        Assert.notNull(loopByPageParams,
                "Pagination parameters are not defined for the `LoopByPage` node {0}!", flowNode.getName());
        Assert.notBlank(loopByPageParams.getModel(),
                "The model name parameter of the `LoopByPage` node {0} cannot be empty!", flowNode.getName());
        Assert.notEmpty(loopByPageParams.getFields(),
                "The field names parameter of the `LoopByPage` node {0} cannot be empty!", flowNode.getName());
        Assert.notBlank(loopByPageParams.getPageParamNaming(),
                "The loop parameter naming of the `LoopByPage` node {0} cannot be blank!", flowNode.getName());
        // Use DEFAULT_PAGE_SIZE when the page size is not configured.
        if (loopByPageParams.getPageSize() == null || loopByPageParams.getPageSize() < 1) {
            loopByPageParams.setPageSize(DEFAULT_PAGE_SIZE);
        } else {
            Assert.isTrue(loopByPageParams.getPageSize() <= MAX_BATCH_SIZE,
                    "The page size {0} of the `LoopByPage` node {1} cannot exceed the maximum limit: {2}",
                    loopByPageParams.getPageSize(), flowNode.getName(), MAX_BATCH_SIZE);
        }
        return loopByPageParams;
    }

    /**
     * Process flow node.
     * The actions in the same node are processed in the same transaction.
     *
     * @param flowNode flow node
     * @param nodeContext environment variables for executing flow actions, including row data that trigger the flow
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void processFlowNode(FlowNode flowNode, NodeContext nodeContext) {
        // Skip the current node and continue to the next node when the execution condition is not met.
        if (!isValidNodeCondition(flowNode.getNodeCondition(), nodeContext)) {
            nodeContext.put(flowNode.getCode(), null);
            return;
        }

        // Iterate through the node type to execute the node list in LoopByDataset/LoopByPage;
        // otherwise, execute the node list once.
        if (FlowNodeType.LOOP_BY_DATASET.equals(flowNode.getNodeType())) {
            this.executeLoopByDataset(flowNode, nodeContext);
        } else if (FlowNodeType.LOOP_BY_PAGE.equals(flowNode.getNodeType())) {
            this.executeLoopByPage(flowNode, nodeContext);
        } else {
            this.executeNodeActions(flowNode, nodeContext);
        }
    }

    /**
     * Execute the `LoopByDataset` node, iterating through to execute the node actions.
     *
     * @param flowNode flow node
     * @param nodeContext environment variables for executing flow actions, including row data that trigger the flow
     */
    private void executeLoopByDataset(FlowNode flowNode, NodeContext nodeContext) {
        LoopByDatasetParams loopByDatasetParams = this.extractLoopByDatasetParams(flowNode);
        Object dataSet = StringTools.extractVariable(loopByDatasetParams.getDataSetParam(), nodeContext.getEnv());
        if (dataSet instanceof Collection) {
            // Iterate over the collection object and execute the node list.
            List<Object> returnList = new ArrayList<>();
            for (Object item : (Collection<?>) dataSet) {
                // Assign `nodeContext` as a local variable within the loop to achieve variable isolation within the loop.
                NodeContext innerNodeContext = nodeContext.copy();
                innerNodeContext.setInLoop(true);
                innerNodeContext.put(loopByDatasetParams.getLoopItemNaming(), item);
                this.executeNodeActions(flowNode, innerNodeContext);
                if (NodeExceptionSignal.END_LOOP_NODE.equals(innerNodeContext.getExceptionSignal())
                        || NodeExceptionSignal.END_FLOW.equals(innerNodeContext.getExceptionSignal())) {
                    // End the loop node and return directly without iterating through the nodes.
                    return;
                } else if (innerNodeContext.getReturnData() != null) {
                    returnList.add(innerNodeContext.getReturnData());
                }
            }
            // Set the merged return value after the iteration ends.
            if (!returnList.isEmpty()) {
                nodeContext.setReturnData(returnList);
            }
        } else {
            throw new IllegalArgumentException(
                    "In the `LoopByDataset` node {0}, the value of dataset parameter {0} must be a collection object! {2}",
                    flowNode.getName(), loopByDatasetParams.getDataSetParam(), dataSet);
        }
    }

    /**
     * Execute the `LoopByPage` node, iterating through to execute the node actions.
     *
     * @param flowNode flow node
     * @param nodeContext environment variables for executing flow actions, including row data that trigger the flow
     */
    private void executeLoopByPage(FlowNode flowNode, NodeContext nodeContext) {
        LoopByPageParams loopByPageParams = this.extractLoopByPageParams(flowNode);
        Filters clonedFilters = null;
        if (!Filters.isEmpty(loopByPageParams.getFilters())) {
            // Resolve variables and formulas in filters.
            clonedFilters = loopByPageParams.getFilters().deepCopy();
            FlowUtils.resolveFilterValue(loopByPageParams.getModel(), clonedFilters, nodeContext);
        }
        // Retrieve the total number of data to avoid counting again within the paginated queries.
        Long total = modelService.count(loopByPageParams.getModel(), clonedFilters);
        if (total == 0) {
            return;
        }
        // Build paginated query conditions in an ordered manner without automatically counting.
        Page<Map<String, Object>> page = Page.of(BaseConstant.DEFAULT_PAGE_NUMBER, loopByPageParams.getPageSize(), false);
        page.setTotal(total);
        FlexQuery flexQuery = new FlexQuery(loopByPageParams.getFields(), clonedFilters, loopByPageParams.getOrders());
        List<Object> returnList = new ArrayList<>();
        // Iterate through paginated queries and execute the node list.
        do {
            page = modelService.searchPage(loopByPageParams.getModel(), flexQuery, page);
            if (page.getRows().isEmpty()) {
                break;
            }
            // Assign `nodeContext` as a local variable within the loop to achieve variable isolation inside the loop.
            NodeContext innerNodeContext = nodeContext.copy();
            innerNodeContext.setInLoop(true);
            // Set the paginated query results to the loop's named parameters.
            innerNodeContext.put(loopByPageParams.getPageParamNaming(), page.getRows());
            this.executeNodeActions(flowNode, innerNodeContext);
            if (NodeExceptionSignal.END_LOOP_NODE.equals(innerNodeContext.getExceptionSignal())
                    || NodeExceptionSignal.END_FLOW.equals(innerNodeContext.getExceptionSignal())) {
                // End the `LoopByPage` node and return directly without further node iteration.
                return;
            } else if (innerNodeContext.getReturnData() != null) {
                returnList.add(innerNodeContext.getReturnData());
            }
        } while (page.toNext());
        // Set the merged return value after the iteration ends.
        if (!returnList.isEmpty()) {
            nodeContext.setReturnData(returnList);
        }
    }

    /**
     * Execute the node node list.
     *
     * @param flowNode flow node
     * @param nodeContext environment variables for executing flow actions, including row data that trigger the flow
     */
    private void executeNodeActions(FlowNode flowNode, NodeContext nodeContext) {
        StopWatch stopWatch = new StopWatch("Executing the node " + flowNode.getName());
        for (FlowNode child : flowNode.getChildNodes()) {
            stopWatch.start(child.getNodeType().getName() + " - " + child.getName());
            nodeFactory.executeNodeProcessor(flowNode, nodeContext);
            stopWatch.stop();
            if (nodeContext.getExceptionSignal() != null) {
                // If an exception signal occurs in the node execution result,
                // return directly without executing the following actions of the current node.
                log.info(stopWatch.prettyPrint());
                return;
            }
        }
        log.info(stopWatch.prettyPrint());
    }
}