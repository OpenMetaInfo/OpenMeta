package info.openmeta.starter.flow.message;

import info.openmeta.starter.flow.message.dto.FlowAsyncTaskMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.client.producer.SendCallback;
import org.apache.rocketmq.client.producer.SendResult;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * Flow async task producer
 */
@Slf4j
@Component
public class FlowAsyncTaskProducer {

    @Value("${rocketmq.topics.flow-async-task.topic}")
    private String asyncTaskTopic;

    @Autowired
    private RocketMQTemplate rocketMQTemplate;

    /**
     * Send the async task of flow to MQ
     */
    public void sendFlowTask(FlowAsyncTaskMessage message) {
        rocketMQTemplate.asyncSend(asyncTaskTopic, message, new SendCallback() {
            @Override
            public void onSuccess(SendResult sendResult) {
                // Send success
            }

            @Override
            public void onException(Throwable throwable) {
                log.error("Failed to send flow async task to MQ!", throwable);
            }
        });
    }
}
