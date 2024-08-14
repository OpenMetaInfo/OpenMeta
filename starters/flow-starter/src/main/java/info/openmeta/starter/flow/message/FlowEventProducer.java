package info.openmeta.starter.flow.message;

import info.openmeta.starter.flow.message.dto.FlowEventMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.client.producer.SendCallback;
import org.apache.rocketmq.client.producer.SendResult;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * Flow event producer
 */
@Slf4j
@Component
public class FlowEventProducer {

    @Value("${rocketmq.topics.flow-event}")
    private String flowEventTopic;

    @Autowired
    private RocketMQTemplate rocketMQTemplate;

    /**
     * Send flow event to MQ.
     */
    public void sendFlowEvent(FlowEventMessage message) {
        rocketMQTemplate.asyncSend(flowEventTopic, message, new SendCallback() {
            @Override
            public void onSuccess(SendResult sendResult) {
                // Send success
            }

            @Override
            public void onException(Throwable throwable) {
                log.error("Failed to send flow event to MQ!", throwable);
            }
        });
    }
}
