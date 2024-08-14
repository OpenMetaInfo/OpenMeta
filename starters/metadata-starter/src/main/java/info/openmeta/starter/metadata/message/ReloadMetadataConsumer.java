package info.openmeta.starter.metadata.message;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.starter.metadata.enums.MetadataReloadType;
import info.openmeta.starter.metadata.message.dto.ReloadMetadataMessage;
import info.openmeta.starter.metadata.service.SysModelService;
import info.openmeta.starter.metadata.service.SysOptionSetService;
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
public class ReloadMetadataConsumer implements RocketMQListener<ReloadMetadataMessage> {

    @Autowired
    private SysModelService sysModelService;

    @Autowired
    private SysOptionSetService optionSetService;

    @Override
    public void onMessage(ReloadMetadataMessage reloadMetadataMessage) {
        ContextHolder.setContext(reloadMetadataMessage.getContext());
        if (MetadataReloadType.RELOAD_MODEL_MANAGER.equals(reloadMetadataMessage.getReloadType())) {
            sysModelService.reloadModelManager();
        } else if (MetadataReloadType.RELOAD_OPTION_MANAGER.equals(reloadMetadataMessage.getReloadType())) {
            optionSetService.reloadOptionManager();
        }
    }

}
