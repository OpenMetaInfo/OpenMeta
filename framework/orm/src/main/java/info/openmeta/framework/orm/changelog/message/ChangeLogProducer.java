package info.openmeta.framework.orm.changelog.message;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.orm.changelog.message.dto.ChangeLog;
import info.openmeta.framework.orm.changelog.message.dto.ChangeLogMessage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.rocketmq.client.producer.SendCallback;
import org.apache.rocketmq.client.producer.SendResult;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * ChangeLog Producer, send ChangeLog to MQ
 */
@Slf4j
@Component
public class ChangeLogProducer {

    @Value("${rocketmq.topics.change-log:}")
    private String changeLogTopic;

    /**
     * Message delay level, default is 1, which means 1s
     */
    @Value("${rocketmq.changelog.delay-level:1}")
    private Integer delayLevel;

    @Autowired
    private RocketMQTemplate rocketMQTemplate;

    /**
     * Send ChangeLog to MQ in batches to avoid exceeding the message size limit
     */
    public void sendChangeLog(List<ChangeLog> changeLogs) {
        if (CollectionUtils.isEmpty(changeLogs)) {
            return;
        } else if (StringUtils.isBlank(changeLogTopic)) {
            log.warn("rocketmq.topics.change-log not configured!");
            return;
        }
        Context clonedContext = ContextHolder.cloneContext();
        for (int i = 0; i < changeLogs.size(); i += BaseConstant.DEFAULT_PAGE_SIZE) {
            List<ChangeLog> changeLogBatch = changeLogs.subList(i, Math.min(i + BaseConstant.DEFAULT_PAGE_SIZE, changeLogs.size()));
            // Create ChangeLogMessage
            ChangeLogMessage changeLogMessage = new ChangeLogMessage(changeLogBatch, clonedContext);
            // Send ChangeLogMessage asynchronously
            Message<?> message = MessageBuilder.withPayload(changeLogMessage).build();
            delayLevel = delayLevel == null ? 1 : delayLevel;
            rocketMQTemplate.asyncSend(changeLogTopic, message, getSendCallback(), 3000, delayLevel);
        }
    }

    private SendCallback getSendCallback() {
        return new SendCallback() {
            @Override
            public void onSuccess(SendResult sendResult) {
                // Send successfully
            }

            @Override
            public void onException(Throwable throwable) {
                log.error("ChangeLog send to MQ failed: ", throwable);
            }
        };
    }
}
