package info.openmeta.starter.flow.action.processors;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.ReturnDataParams;
import info.openmeta.starter.flow.utils.FlowUtils;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import org.springframework.stereotype.Component;

import java.util.Map;

/**
 * Processor for ReturnData action.
 * Return the data to the action context.
 */
@Component
public class ReturnDataAction implements ActionProcessor<ReturnDataParams> {

    @Override
    public FlowActionType getActionType() {
        return FlowActionType.RETURN_DATA;
    }

    @Override
    public Class<ReturnDataParams> getParamsClass() {
        return ReturnDataParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, ReturnDataParams actionParams) {
        Assert.notEmpty(actionParams.getDataTemplate(),
                "The data template configuration for ReturnDataAction {0} cannot be blank.",
                flowAction.getName());
    }

    /**
     * Execute the ReturnDataAction processor.
     * The value supports constants, variables, and calculation formulas,
     * where variables are represented by `#{}` and calculation formulas are represented by `${}`.
     * Example:
     * <p>
     * {
     *     "dataTemplate":  {
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
    public void execute(FlowAction flowAction, ReturnDataParams actionParams, ActionContext actionContext) {
        Map<String, Object> dataMap = FlowUtils.resolveDataTemplate(actionParams.getDataTemplate(), actionContext);
        actionContext.setReturnData(dataMap);
    }

}
