package info.openmeta.starter.flow.action.processors;

import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.DeleteDataParams;
import info.openmeta.starter.flow.utils.FlowUtils;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.util.Collection;

import static info.openmeta.framework.orm.constant.ModelConstant.ID;

/**
 * Processor for DeleteData action.
 * Delete data based on the specified primary key or filter conditions.
 */
@Slf4j
@Component
public class DeleteDataAction implements ActionProcessor<DeleteDataParams> {

    @Autowired
    private ModelService<? extends Serializable> modelService;

    /**
     * Get the FlowActionType processed by the current processor.
     *
     * @return The FlowActionType associated with the current processor.
     */
    @Override
    public FlowActionType getActionType() {
        return FlowActionType.DELETE_DATA;
    }

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    @Override
    public Class<DeleteDataParams> getParamsClass() {
        return DeleteDataParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, DeleteDataParams actionParams) {
        Assert.notBlank(actionParams.getModelName(),
                "The model name for Delete Action {0} cannot be empty!", flowAction.getName());
        Assert.notTrue(StringUtils.isBlank(actionParams.getPkVariable()) && Filters.isEmpty(actionParams.getFilters()),
                "In the parameter configuration of the Delete Action {0}, at least " +
                        "the primary key parameter or the filter condition for deleting data must be specified.",
                flowAction.getName());
    }

    /**
     * Execute the Delete Action processor.
     * Either the primary key or filters must be provided.
     * When both appear at the same time, merge the primary key and filters with AND.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     * @param actionContext The action context
     */
    @Override
    public void execute(FlowAction flowAction, DeleteDataParams actionParams, ActionContext actionContext) {
        String pkVariable = actionParams.getPkVariable();
        Filters deleteFilters = null;
        if (StringTools.isVariable(pkVariable)) {
            // String variable parameter `#{}`: retrieve ids from the actionContext
            Collection<?> ids = FlowUtils.getIdsFromPkVariable(flowAction, pkVariable, actionContext);
            if (CollectionUtils.isEmpty(ids)) {
                return;
            }
            deleteFilters = Filters.in(ID, ids);
        }
        // Merge primary key list with filters to form the final delete conditions
        if (!Filters.isEmpty(actionParams.getFilters())) {
            // Convert variables and calculation formulas in filters
            Filters clonedFilters = actionParams.getFilters().deepCopy();
            FlowUtils.resolveFilterValue(actionParams.getModelName(), clonedFilters, actionContext);
            deleteFilters = Filters.merge(deleteFilters, clonedFilters);
        }
        if (Filters.isEmpty(deleteFilters)) {
            return;
        }
        modelService.deleteByFilters(actionParams.getModelName(), deleteFilters);
    }
}
