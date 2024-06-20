package info.openmeta.framework.orm.changelog.message;

import info.openmeta.framework.orm.changelog.message.dto.CommonLogMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.rocketmq.client.producer.SendCallback;
import org.apache.rocketmq.client.producer.SendResult;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * Common log producer, send common log to MQ
 */
@Slf4j
@Component
public class CommonLogProducer {

    @Value("${rocketmq.topics.common-log:}")
    private String commonLogTopic;

    @Autowired
    private RocketMQTemplate rocketMQTemplate;

    /**
     * Send common log to MQ
     */
    public void sendCommonLog(CommonLogMessage message) {
        if (StringUtils.isBlank(commonLogTopic)) {
            log.warn("rocketmq.topics.common-log not configured!");
            return;
        }
        rocketMQTemplate.asyncSend(commonLogTopic, message, new SendCallback() {
            @Override
            public void onSuccess(SendResult sendResult) {
                // Successfully sent
            }

            @Override
            public void onException(Throwable throwable) {
                log.error("Failed to send common log to MQ.", throwable);
            }
        });
    }

}
