package info.openmeta.starter.file.message;

import info.openmeta.starter.file.dto.ImportTemplateDTO;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.client.producer.SendCallback;
import org.apache.rocketmq.client.producer.SendResult;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * AsyncImportProducer
 */
@Slf4j
@Component
public class AsyncImportProducer {

    @Value("${rocketmq.topics.async-import.topic}")
    private String importTopic;

    @Autowired
    private RocketMQTemplate rocketMQTemplate;

    /**
     * Send asynchronous import task message.
     */
    public void sendAsyncImport(ImportTemplateDTO message) {
        rocketMQTemplate.asyncSend(importTopic, message, new SendCallback() {
            @Override
            public void onSuccess(SendResult sendResult) {
                log.debug("The asynchronous import message was successfully sent: {}", message);
            }

            @Override
            public void onException(Throwable e) {
                log.error("Failed to send an asynchronous import message: {}", message, e);
            }
        });
    }

}
