package info.openmeta.starter.metadata.message;

import info.openmeta.starter.metadata.message.dto.InnerBroadcastMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.rocketmq.client.producer.SendCallback;
import org.apache.rocketmq.client.producer.SendResult;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * Reload metadata producer
 */
@Slf4j
@Component
public class InnerBroadcastProducer {

    @Value("${rocketmq.topics.inner-broadcast.topic:}")
    private String innerBroadcastTopic;

    @Autowired
    private RocketMQTemplate rocketMQTemplate;

    /**
     * Send an inner broadcast message to MQ.
     */
    public void sendInnerBroadcast(InnerBroadcastMessage message) {
        if (StringUtils.isBlank(innerBroadcastTopic)) {
            log.warn("rocketmq.topics.inner-broadcast.topic not configured!");
            return;
        }
        rocketMQTemplate.asyncSend(innerBroadcastTopic, message, new SendCallback() {
            @Override
            public void onSuccess(SendResult sendResult) {
                // Success
            }

            @Override
            public void onException(Throwable throwable) {
                log.error("Failed to send inner broadcast message to MQ!", throwable);
            }
        });
    }
}
