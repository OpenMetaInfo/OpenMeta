package info.openmeta.starter.flow.action.processors;

import info.openmeta.starter.ai.entity.AiMessage;
import info.openmeta.starter.ai.service.AiRobotService;
import info.openmeta.starter.ai.vo.ChatMessage;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.compute.ComputeUtils;
import info.openmeta.starter.flow.action.ActionContext;
import info.openmeta.starter.flow.action.ActionProcessor;
import info.openmeta.starter.flow.action.params.QueryAiParams;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.enums.FlowActionType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Processor for QueryAi action.
 * Query AI and put the result into the action context.
 */
@Component
public class QueryAiAction implements ActionProcessor<QueryAiParams> {

    @Autowired
    private AiRobotService robotService;

    @Override
    public FlowActionType getActionType() {
        return FlowActionType.QUERY_AI;
    }

    @Override
    public Class<QueryAiParams> getParamsClass() {
        return QueryAiParams.class;
    }

    /**
     * Validate the parameters of the specified FlowAction under the current action processor.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     */
    @Override
    public void validateParams(FlowAction flowAction, QueryAiParams actionParams) {
        Assert.notBlank(actionParams.getQuery(),
                "The query parameter for Query AI Action {0} cannot be empty.", flowAction.getName());
    }

    /**
     * Execute the QueryAiAction processor.
     * Query content supports string interpolation `#{var}`.
     *
     * @param flowAction The flow action
     * @param actionParams The parameters of the flow action.
     * @param actionContext The action context
     */
    @Override
    public void execute(FlowAction flowAction, QueryAiParams actionParams, ActionContext actionContext) {
        // Compute string interpolation
        String query = ComputeUtils.stringInterpolation(actionParams.getQuery(), actionContext.getEnv());
        ChatMessage chatMessage = new ChatMessage(actionParams.getRobotId(), actionParams.getConversationId(), query);
        AiMessage aiMessage = robotService.chat(chatMessage);
        // Put the AI reply content into the Action context
        actionContext.put(flowAction.getCode(), aiMessage.getAnswer());
    }
}
