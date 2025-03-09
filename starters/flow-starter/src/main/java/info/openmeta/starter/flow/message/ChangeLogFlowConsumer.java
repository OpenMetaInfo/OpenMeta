package info.openmeta.starter.flow.message;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.orm.changelog.message.dto.ChangeLogMessage;
import info.openmeta.starter.flow.FlowAutomation;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Change log consumer for Flow
 */
@Component
@RocketMQMessageListener(topic = "${rocketmq.topics.change-log.topic}", consumerGroup = "${rocketmq.topics.change-log.flow-group}")
public class ChangeLogFlowConsumer implements RocketMQListener<ChangeLogMessage> {

    @Autowired
    private FlowAutomation flowAutomation;

    @Override
    public void onMessage(ChangeLogMessage changeLogMessage) {
        ContextHolder.setContext(changeLogMessage.getContext());
        if (ContextHolder.getContext().isTriggerFlow()) {
            flowAutomation.triggerAsyncFlowByChangeLog(changeLogMessage.getChangeLogs());
        }
    }

}
