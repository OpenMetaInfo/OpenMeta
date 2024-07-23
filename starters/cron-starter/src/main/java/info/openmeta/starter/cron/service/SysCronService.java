package info.openmeta.starter.cron.service;

import info.openmeta.framework.orm.service.EntityService;
import info.openmeta.starter.cron.entity.SysCron;

import java.util.List;

/**
 * SysCron Model Service Interface
 */
public interface SysCronService extends EntityService<SysCron, Long> {

    /**
     * Activate one cron job.
     *
     * @param cronId Cron ID
     */
    void activateCron(Long cronId);

    /**
     * Activate multiple cron jobs.
     *
     * @param cronIds Cron IDs
     */
    void activateCronList(List<Long> cronIds);

    /**
     * Execute the specified cron job immediately
     *
     * @param cronId Cron job ID
     */
    void executeNow(Long cronId);

    /**
     * Execute the specified multiple cron jobs immediately
     *
     * @param cronIds Cron job IDs
     */
    void executeMultipleNow(List<Long> cronIds);

}