package info.openmeta.starter.cron.scheduler;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

/**
 * Scheduler thread pool size, default is 1.
 * Only used for cron scheduler, not for task execution.
 */
@Component
public class SchedulerPool {

    @Value("${cron.threads.number:1}")
    private Integer cronThreadsNumber;

    private ScheduledExecutorService scheduler;

    /**
     * Get scheduler instance.
     */
    public ScheduledExecutorService getScheduler() {
        if (this.scheduler == null) {
            this.scheduler = Executors.newScheduledThreadPool(cronThreadsNumber);
        }
        return this.scheduler;
    }
}
