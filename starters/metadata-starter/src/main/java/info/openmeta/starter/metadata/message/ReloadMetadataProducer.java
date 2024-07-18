package info.openmeta.starter.metadata.message;

import info.openmeta.starter.metadata.message.dto.ReloadMetadataMessage;
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
public class ReloadMetadataProducer {

    @Value("${rocketmq.topics.reload-metadata:}")
    private String reloadMetadataTopic;

    @Autowired
    private RocketMQTemplate rocketMQTemplate;

    /**
     * Send reload metadata message to MQ.
     */
    public void sendInnerBroadcast(ReloadMetadataMessage message) {
        if (StringUtils.isBlank(reloadMetadataTopic)) {
            log.warn("rocketmq.topics.reload-metadata not configured!");
            return;
        }
        rocketMQTemplate.asyncSend(reloadMetadataTopic, message, new SendCallback() {
            @Override
            public void onSuccess(SendResult sendResult) {
                // Success
            }

            @Override
            public void onException(Throwable throwable) {
                log.error("Failed to send reload metadata message to MQ!", throwable);
            }
        });
    }
}
