package info.openmeta.starter.flow.message;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.starter.flow.message.dto.FlowEventMessage;
import info.openmeta.starter.flow.service.FlowConfigService;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * Flow event consumer
 */
@Component
@RocketMQMessageListener(topic = "${rocketmq.topics.flow-event.topic}", consumerGroup = "${rocketmq.topics.flow-event.group}")
public class FlowEventConsumer implements RocketMQListener<FlowEventMessage> {

    @Autowired
    private FlowConfigService flowConfigService;

    @Value("${enable.flow:true}")
    private Boolean enableFlow;

    /**
     * Flow event message consumption, persist the flow event,
     * and trigger the related flow according to the flow configuration.
     *
     * @param eventMessage Event message
     */
    @Override
    public void onMessage(FlowEventMessage eventMessage) {
        if (!enableFlow) {
            return;
        }
        ContextHolder.setContext(eventMessage.getContext());
        if (Boolean.TRUE.equals(eventMessage.getRollbackOnFail())) {
            // Transactional Flow
            flowConfigService.executeTransactionalFlow(eventMessage);
        } else {
            flowConfigService.executeFlow(eventMessage);
        }
    }

}
