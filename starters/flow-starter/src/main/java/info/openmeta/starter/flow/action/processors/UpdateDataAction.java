package info.openmeta.starter.flow.action.processors;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.UpdateDataParams;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import info.openmeta.starter.flow.utils.FlowUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.util.Collection;
import java.util.Map;

import static info.openmeta.framework.orm.constant.ModelConstant.ID;

/**
 * Processor for UpdateData action.
 * Update data based on the specified model, primary key or filters, and row template.
 */
@Component
public class UpdateDataAction implements ActionProcessor<UpdateDataParams> {

    @Autowired
    private ModelService<? extends Serializable> modelService;

    @Override
    public FlowActionType getActionType() {
        return FlowActionType.UPDATE_DATA;
    }

    @Override
    public Class<UpdateDataParams> getParamsClass() {
        return UpdateDataParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, UpdateDataParams actionParams) {
        Assert.notEmpty(actionParams.getRowTemplate(),
                "The model name parameter for Update Action {0} cannot be blank!", flowAction.getName());
        Assert.notTrue(StringUtils.isBlank(actionParams.getPkVariable()) && Filters.isEmpty(actionParams.getFilters()),
                """
                The parameter configuration for UpdateDataAction {0} must specify at least
                the primary key parameter or the update data filtering conditions.
                """, flowAction.getName());
    }

    /**
     * Execute the UpdateDataAction processor.
     * The value supports constants, variables, and calculation formulas,
     * where variables are represented by `#{}` and calculation formulas are represented by `${}`.
     * Example:
     * <p>
     * {
     *     "modelName": "SysModel",
     *     "pkVariable": "#{deptId}",
     *     "filters": ["code", "=", "#{deptCode}"],
     *     "rowTemplate":  {
     *         "parentId": "#{parentId}",
     *         "name": "#{deptName}",
     *         "ownId": "#{ownId}"
     *     }
     * }
     * </p>
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     * @param actionContext The action context
     */
    @Override
    public void execute(FlowAction flowAction, UpdateDataParams actionParams, ActionContext actionContext) {
        String pkVariable = actionParams.getPkVariable();
        Filters updateFilters = null;
        if (StringTools.isVariable(pkVariable)) {
            // String variable parameter `#{}`: retrieve ids from the actionContext
            Collection<?> ids = FlowUtils.getIdsFromPkVariable(flowAction, pkVariable, actionContext);
            if (CollectionUtils.isEmpty(ids)) {
                return;
            }
            updateFilters = Filters.in(ID, ids);
        }
        // Merge primary key list with filters to form the final delete conditions
        if (!Filters.isEmpty(actionParams.getFilters())) {
            // Convert variables and calculation formulas in filters
            Filters clonedFilters = actionParams.getFilters().deepCopy();
            FlowUtils.resolveFilterValue(actionParams.getModel(), clonedFilters, actionContext);
            updateFilters = Filters.merge(updateFilters, clonedFilters);
        }
        if (Filters.isEmpty(updateFilters)) {
            return;
        }
        // Generate updated data based on the data template
        Map<String, Object> rowMap = FlowUtils.resolveRowTemplate(actionParams.getModel(), actionParams.getRowTemplate(), actionContext);
        modelService.updateByFilter(actionParams.getModel(), updateFilters, rowMap);
    }

}
