package info.openmeta.framework.web.message;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.meta.OptionManager;
import info.openmeta.framework.web.message.dto.InnerBroadcastMessage;
import info.openmeta.framework.web.message.enums.InnerBroadcastType;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.annotation.MessageModel;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * In-app broadcast consumer, using MQ broadcast mode, all container replicas execute broadcast consumption.
 */
@Slf4j
@Component
@RocketMQMessageListener(messageModel = MessageModel.BROADCASTING, topic = "${rocketmq.topics.reload-metadata}", consumerGroup = "${rocketmq.topics.reload-metadata}" + "_0")
public class InnerBroadcastConsumer implements RocketMQListener<InnerBroadcastMessage> {

    @Autowired
    private ModelManager modelManager;

    @Autowired
    private OptionManager optionManager;

    @Override
    public void onMessage(InnerBroadcastMessage message) {
        ContextHolder.setContext(message.getContext());
        if (InnerBroadcastType.RELOAD_METADATA.equals(message.getBroadcastType())) {
            modelManager.init();
            optionManager.init();
        } else {
            log.warn("Unknown broadcast type: {}", message.getBroadcastType());
        }
    }

}
