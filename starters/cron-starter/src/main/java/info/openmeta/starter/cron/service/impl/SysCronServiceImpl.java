package info.openmeta.starter.cron.service.impl;

import info.openmeta.framework.orm.compute.CronUtils;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.cron.scheduler.CronScheduler;
import info.openmeta.starter.cron.entity.SysCron;
import info.openmeta.starter.cron.service.SysCronService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * SysCron Model Service Implementation
 */
@Service
public class SysCronServiceImpl extends EntityServiceImpl<SysCron, Long> implements SysCronService {

    @Autowired
    @Lazy
    private CronScheduler cronScheduler;

    /**
     * Activate one cron job.
     *
     * @param cronId Cron ID
     */
    @Override
    public void activateCron(Long cronId) {
        // Cancel the existing cron job with the same ID first (if any).
        cronScheduler.cancelTask(cronId);
        // Schedule the new cron job.
        SysCron sysCron = this.readOne(cronId);
        cronScheduler.registerCron(sysCron);
    }

    /**
     * Activate multiple cron jobs.
     *
     * @param cronIds Cron IDs
     */
    @Override
    public void activateCronList(List<Long> cronIds) {
        // Cancel the existing cron jobs with the same IDs first (if any).
        cronScheduler.cancelTaskList(cronIds);
        // Schedule the new cron jobs.
        List<SysCron> sysCronList = this.readList(cronIds);
        for (SysCron sysCron : sysCronList) {
            cronScheduler.registerCron(sysCron);
        }
    }

    /**
     * Run the specified cron job immediately
     *
     * @param cronId Cron job ID
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void runNow(Long cronId) {
        SysCron sysCron = this.readOne(cronId);
        CronUtils.getCron(sysCron.getName(), sysCron.getCronExpression());
        cronScheduler.sendToMessageQueue(sysCron);
    }

    /**
     * Run the specified multiple cron jobs immediately
     *
     * @param cronIds Cron job IDs
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void runNow(List<Long> cronIds) {
        List<SysCron> sysCronList = this.readList(cronIds);
        for (SysCron sysCron : sysCronList) {
            this.runNow(sysCron.getId());
        }
    }

}