package info.openmeta.starter.flow.message;

import info.openmeta.framework.base.enums.SystemUser;
import info.openmeta.framework.orm.annotation.SwitchUser;
import info.openmeta.starter.cron.entity.SysCronLog;
import info.openmeta.starter.cron.message.dto.CronTaskMessage;
import info.openmeta.starter.cron.service.SysCronLogService;
import info.openmeta.starter.flow.FlowAutomation;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Cron task consumer for Flow
 */
@Component
@RocketMQMessageListener(topic = "${rocketmq.topics.cron-task.topic}", consumerGroup = "${rocketmq.topics.cron-task.flow-group}")
public class CronTaskFlowConsumer implements RocketMQListener<CronTaskMessage> {

    @Autowired
    private FlowAutomation flowAutomation;

    @Autowired
    private SysCronLogService cronLogService;

    @Override
    @SwitchUser(SystemUser.CRON_USER)
    public void onMessage(CronTaskMessage cronTaskMessage) {
        persistCronLog(cronTaskMessage);
        flowAutomation.cronEvent(cronTaskMessage);
    }

    /**
     * Persist the cron task execution log
     *
     * @param cronTaskMessage Cron task message
     */
    private void persistCronLog(CronTaskMessage cronTaskMessage) {
        SysCronLog cronLog = new SysCronLog();
        cronLog.setCronId(cronTaskMessage.getCronId());
        cronLog.setCronName(cronTaskMessage.getCronName());
        cronLog.setStartTime(cronTaskMessage.getTriggerTime());
        cronLogService.createOne(cronLog);
    }
}