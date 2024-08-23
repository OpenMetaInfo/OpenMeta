package info.openmeta.starter.cron.message;

import info.openmeta.starter.cron.message.dto.CronTaskMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.client.producer.SendCallback;
import org.apache.rocketmq.client.producer.SendResult;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * Cron task producer.
 */
@Slf4j
@Component
public class CronTaskProducer {

    @Value("${rocketmq.topics.cron-task}")
    private String cronTaskTopic;

    @Autowired
    private RocketMQTemplate rocketMQTemplate;

    /**
     * Send cron task message.
     */
    public void sendCronTask(CronTaskMessage message) {
        rocketMQTemplate.asyncSend(cronTaskTopic, message, new SendCallback() {
            @Override
            public void onSuccess(SendResult sendResult) {
                log.debug("Cron scheduler successfully sends task execution MQ: {}", message);
            }

            @Override
            public void onException(Throwable throwable) {
                log.error("Cron scheduler failed to send task execution MQ: {}", message, throwable);
            }
        });
    }
}
