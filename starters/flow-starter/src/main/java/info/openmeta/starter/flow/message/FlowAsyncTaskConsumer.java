package info.openmeta.starter.flow.message;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.web.task.AsyncTaskFactory;
import info.openmeta.starter.flow.message.dto.FlowAsyncTaskMessage;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Flow async task consumer, also the async task executor
 */
@Component
@RocketMQMessageListener(topic = "${rocketmq.topics.flow-async-task.topic}", consumerGroup = "${rocketmq.topics.flow-async-task.group}")
public class FlowAsyncTaskConsumer implements RocketMQListener<FlowAsyncTaskMessage> {

    @Autowired
    private AsyncTaskFactory<?> asyncTaskFactory;

    @Override
    public void onMessage(FlowAsyncTaskMessage flowAsyncTaskMessage) {
        ContextHolder.setContext(flowAsyncTaskMessage.getContext());
        asyncTaskFactory.executeAsyncTask(flowAsyncTaskMessage.getAsyncTaskHandlerCode(), flowAsyncTaskMessage.getAsyncTaskParams());
    }

}
