package info.openmeta.starter.flow.action.processors;

import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.CreateDataParams;
import info.openmeta.starter.flow.utils.FlowUtils;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.util.Map;

/**
 * Processor for CreateData action.
 * Create data based on the specified data template.
 */
@Component
public class CreateDataAction implements ActionProcessor<CreateDataParams> {

    @Autowired
    private ModelService<? extends Serializable> modelService;

    /**
     * Get the FlowActionType processed by the current processor.
     *
     * @return The FlowActionType associated with the current processor.
     */
    @Override
    public FlowActionType getActionType() {
        return FlowActionType.CREATE_DATA;
    }

    /**
     * Get the class type of the parameters required by the current processor.
     *
     * @return The Class of the encapsulated parameters.
     */
    @Override
    public Class<CreateDataParams> getParamsClass() {
        return CreateDataParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, CreateDataParams actionParams) {
        Assert.notBlank(actionParams.getModel(),
                "The model name parameter for action {0} cannot be empty!", flowAction.getName());
        Assert.notEmpty(actionParams.getRowTemplate(),
                "The data template parameter for action {0} cannot be empty!", flowAction.getName());
    }

    /**
     * Execute the CreateDataAction processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     * @param actionContext The action context
     */
    @Override
    public void execute(FlowAction flowAction, CreateDataParams actionParams, ActionContext actionContext) {
        // Generate updated data based on the data template
        Map<String, Object> rowMap = FlowUtils.resolveRowTemplate(actionParams.getModel(), actionParams.getRowTemplate(), actionContext);
        modelService.createOne(actionParams.getModel(), rowMap);
    }
}
