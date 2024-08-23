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
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.enums.ActionExceptionSignal;
import info.openmeta.starter.flow.enums.FlowNodeType;
import info.openmeta.starter.flow.node.LoopByDatasetParams;
import info.openmeta.starter.flow.node.LoopByPageParams;
import info.openmeta.starter.flow.service.FlowActionService;
import info.openmeta.starter.flow.service.FlowNodeService;
import info.openmeta.starter.flow.utils.FlowUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
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
    private FlowActionService flowActionService;

    /**
     * Verify whether the condition expression is met, and then determine whether to execute the current node or action.
     * The Flow Engine has set the env to be unmodifiable to avoid data pollution
     *
     * @param condition condition expression
     * @param actionContext flow environment variables
     * @return whether to execute
     */
    private boolean isValidNodeCondition(String condition, ActionContext actionContext) {
        if (StringUtils.isNotBlank(condition)) {
            return ComputeUtils.executeBoolean(condition, actionContext.getEnv());
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
            loopByDatasetParams = JsonMapper.jsonNodeToObject(flowNode.getLoopParams(), LoopByDatasetParams.class);
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
            loopByPageParams = JsonMapper.jsonNodeToObject(flowNode.getLoopParams(), LoopByPageParams.class);
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
     * @param actionContext environment variables for executing flow actions, including row data that trigger the flow
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void processFlowNode(FlowNode flowNode, ActionContext actionContext) {
        // Skip the current node and continue to the next node when the execution condition is not met.
        if (!isValidNodeCondition(flowNode.getNodeCondition(), actionContext)) {
            return;
        }
        // Skip the current node and continue to the next node when the action list is empty.
        if (CollectionUtils.isEmpty(flowNode.getActionList())) {
            return;
        }
        // Iterate through the node type to execute the action list in LoopByDataset/LoopByPage;
        // otherwise, execute the action list once.
        if (FlowNodeType.LOOP_BY_DATASET.equals(flowNode.getNodeType())) {
            this.executeLoopByDataset(flowNode, actionContext);
        } else if (FlowNodeType.LOOP_BY_PAGE.equals(flowNode.getNodeType())) {
            this.executeLoopByPage(flowNode, actionContext);
        } else {
            this.executeNodeActions(flowNode, actionContext);
        }
    }

    /**
     * Execute the `LoopByDataset` node, iterating through to execute the node actions.
     *
     * @param flowNode flow node
     * @param actionContext environment variables for executing flow actions, including row data that trigger the flow
     */
    private void executeLoopByDataset(FlowNode flowNode, ActionContext actionContext) {
        LoopByDatasetParams loopByDatasetParams = this.extractLoopByDatasetParams(flowNode);
        Object dataSet = FlowUtils.extractVariable(loopByDatasetParams.getDataSetParam(), actionContext);
        if (dataSet instanceof Collection) {
            // Iterate over the collection object and execute the action list.
            List<Object> returnList = new ArrayList<>();
            for (Object item : (Collection<?>) dataSet) {
                // Assign `actionContext` as a local variable within the loop to achieve variable isolation within the loop.
                ActionContext innerActionContext = actionContext.copy();
                innerActionContext.setInLoop(true);
                innerActionContext.put(loopByDatasetParams.getLoopItemNaming(), item);
                this.executeNodeActions(flowNode, innerActionContext);
                if (ActionExceptionSignal.END_LOOP_NODE.equals(innerActionContext.getExceptionSignal())
                        || ActionExceptionSignal.END_FLOW.equals(innerActionContext.getExceptionSignal())) {
                    // End the loop node and return directly without iterating through the nodes.
                    return;
                } else if (innerActionContext.getReturnData() != null) {
                    returnList.add(innerActionContext.getReturnData());
                }
            }
            // Set the merged return value after the iteration ends.
            if (!returnList.isEmpty()) {
                actionContext.setReturnData(returnList);
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
     * @param actionContext environment variables for executing flow actions, including row data that trigger the flow
     */
    private void executeLoopByPage(FlowNode flowNode, ActionContext actionContext) {
        LoopByPageParams loopByPageParams = this.extractLoopByPageParams(flowNode);
        Filters clonedFilters = null;
        if (!Filters.isEmpty(loopByPageParams.getFilters())) {
            // Resolve variables and formulas in filters.
            clonedFilters = loopByPageParams.getFilters().deepCopy();
            FlowUtils.resolveFilterValue(loopByPageParams.getModel(), clonedFilters, actionContext);
        }
        // Retrieve the total number of data to avoid counting again within the paginated queries.
        Long total = modelService.count(loopByPageParams.getModel(), clonedFilters);
        if (total == 0) {
            return;
        }
        // Build paginated query conditions in an ordered manner without automatically counting.
        Page<Map<String, Object>> page = Page.of(BaseConstant.DEFAULT_PAGE_NUMBER, loopByPageParams.getPageSize(), true, false);
        page.setTotal(total);
        FlexQuery flexQuery = new FlexQuery(loopByPageParams.getFields(), clonedFilters, loopByPageParams.getOrders());
        List<Object> returnList = new ArrayList<>();
        // Iterate through paginated queries and execute the action list.
        do {
            page = modelService.searchPage(loopByPageParams.getModel(), flexQuery, page);
            // Assign `actionContext` as a local variable within the loop to achieve variable isolation inside the loop.
            ActionContext innerActionContext = actionContext.copy();
            innerActionContext.setInLoop(true);
            // Set the paginated query results to the loop's named parameters.
            innerActionContext.put(loopByPageParams.getPageParamNaming(), page.getRows());
            this.executeNodeActions(flowNode, innerActionContext);
            if (ActionExceptionSignal.END_LOOP_NODE.equals(innerActionContext.getExceptionSignal())
                    || ActionExceptionSignal.END_FLOW.equals(innerActionContext.getExceptionSignal())) {
                // End the `LoopByPage` node and return directly without further node iteration.
                return;
            } else if (innerActionContext.getReturnData() != null) {
                returnList.add(innerActionContext.getReturnData());
            }
        } while (page.toNext());
        // Set the merged return value after the iteration ends.
        if (!returnList.isEmpty()) {
            actionContext.setReturnData(returnList);
        }
    }

    /**
     * Execute the node action list.
     *
     * @param flowNode flow node
     * @param actionContext environment variables for executing flow actions, including row data that trigger the flow
     */
    private void executeNodeActions(FlowNode flowNode, ActionContext actionContext) {
        StopWatch stopWatch = new StopWatch("Executing the node " + flowNode.getName());
        for (FlowAction flowAction : flowNode.getActionList()) {
            stopWatch.start(flowAction.getActionType().getName() + " - " + flowAction.getName());
            flowActionService.processFlowAction(flowAction, actionContext);
            stopWatch.stop();
            if (actionContext.getExceptionSignal() != null) {
                // If an exception signal occurs in the action execution result,
                // return directly without executing the following actions of the current node.
                log.info(stopWatch.prettyPrint());
                return;
            }
        }
        log.info(stopWatch.prettyPrint());
    }
}