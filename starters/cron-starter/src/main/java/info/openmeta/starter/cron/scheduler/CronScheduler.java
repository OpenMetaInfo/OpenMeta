package info.openmeta.starter.cron.scheduler;

import com.cronutils.model.Cron;
import com.cronutils.model.time.ExecutionTime;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.enums.SystemUser;
import info.openmeta.framework.orm.annotation.SwitchUser;
import info.openmeta.framework.orm.compute.CronUtils;
import info.openmeta.framework.orm.jdbc.JdbcService;
import info.openmeta.starter.cron.entity.SysCron;
import info.openmeta.starter.cron.message.CronTaskProducer;
import info.openmeta.starter.cron.message.dto.CronTaskMessage;
import jakarta.annotation.PreDestroy;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Cron Scheduler
 * Due to the use of leader election, there is only one running scheduler in the entire scheduling system.
 */
@Slf4j
@Component
public class CronScheduler {

    @Autowired
    private SchedulerPool schedulerPool;

    @Autowired
    private JdbcService<?> jdbcService;

    @Autowired
    private CronTaskProducer cronTaskProducer;

    private final Map<Long, ScheduledFuture<?>> scheduledTasksMap = new ConcurrentHashMap<>();

    /**
     * Start all active cron jobs.
     */
    @SwitchUser(SystemUser.CRON_USER)
    public void start() {
        List<SysCron> sysCronList = jdbcService.selectMetaEntityList(SysCron.class, null);
        for (SysCron sysCron : sysCronList) {
            if (!Boolean.TRUE.equals(sysCron.getDisabled())) {
                registerCron(sysCron);
            }
        }
    }

    /**
     * Register a cron job.
     * An active cron job is scheduled in limited execution times or repeatedly.
     *
     * @param sysCron Cron job object
     */
    public void registerCron(SysCron sysCron) {
        Cron cron = CronUtils.getCron(sysCron.getName(), sysCron.getCronExpression());
        ExecutionTime executionTime = ExecutionTime.forCron(cron);
        if (!Boolean.TRUE.equals(sysCron.getLimitExecution())) {
            // Cron with unlimited execution times
            scheduleRepeatedTask(sysCron, executionTime);
        } else if (sysCron.getRemainingCount() > 0){
            // Cron with limited execution times, and there are remaining execution times.
            AtomicInteger remainingCount = new AtomicInteger(sysCron.getRemainingCount());
            scheduleLimitedTask(sysCron, executionTime, remainingCount);
        }
    }

    /**
     * Schedule a task repeatedly.
     * Send an asynchronous execution message in the task, and then reschedule.
     *
     * @param sysCron Cron job object
     * @param executionTime Execution time manager of the cron expression
     */
    private void scheduleRepeatedTask(SysCron sysCron, ExecutionTime executionTime) {
        Runnable task = () -> {
            sendToMessageQueue(sysCron);
            // Reschedule
            scheduleRepeatedTask(sysCron, executionTime);
        };
        scheduleTask(sysCron, executionTime, task);
    }

    /**
     * Schedule a task with limited execution times.
     * Send an asynchronous execution message in the task, and then reschedule.
     *
     * @param sysCron Cron job object
     * @param executionTime Execution time manager of the cron expression
     * @param remainingCount Remaining execution times
     */
    private void scheduleLimitedTask(SysCron sysCron, ExecutionTime executionTime, AtomicInteger remainingCount) {
        Runnable task = () -> {
            // Check the remaining execution times
            if (remainingCount.get() <= 0) {
                return;
            }
            sendToMessageQueue(sysCron);
            remainingCount.decrementAndGet();
            // Reschedule
            scheduleLimitedTask(sysCron, executionTime, remainingCount);
        };
        scheduleTask(sysCron, executionTime, task);
    }

    /**
     * Schedule a task once based on the cron expression.
     * Calculate the next execution time based on the time manager,
     * calculate the delay time, and execute the scheduled task.
     *
     * @param sysCron Cron job object
     * @param executionTime Execution time manager of the cron expression
     * @param task Scheduled task
     */
    private void scheduleTask(SysCron sysCron, ExecutionTime executionTime, Runnable task) {
        ZonedDateTime now = LocalDateTime.now().atZone(ZoneId.systemDefault());
        ZonedDateTime nextExecutionTime = executionTime.nextExecution(now).orElse(null);
        if (nextExecutionTime == null) {
            return;
        }
        // Calculate the time difference between the next execution time and the current time
        // as the delay time of the scheduled task
        long delayMs = Duration.between(now, nextExecutionTime).toMillis();
        ScheduledFuture<?> scheduledTask = schedulerPool.getScheduler().schedule(task, delayMs, TimeUnit.MILLISECONDS);
        scheduledTasksMap.put(sysCron.getId(), scheduledTask);
    }

    /**
     * Send the scheduled task message.
     *
     * @param sysCron Scheduled task object
     */
    public void sendToMessageQueue(SysCron sysCron) {
        CronTaskMessage message = new CronTaskMessage();
        message.setCronId(sysCron.getId());
        message.setCronName(sysCron.getName());
        message.setTriggerTime(LocalDateTime.now());
        message.setLastExecTime(sysCron.getLastExecTime());
        message.setContext(ContextHolder.getContext());
        cronTaskProducer.sendCronTask(message);
    }

    /**
     * Cancel a scheduling task.
     * If the task is being scheduled, close it directly and clear the cache.
     *
     * @param cronId Task ID
     */
    public void cancelTask(Long cronId) {
        ScheduledFuture<?> scheduledTask = scheduledTasksMap.get(cronId);
        if (scheduledTask != null) {
            scheduledTask.cancel(true);
            scheduledTasksMap.remove(cronId);
        }
    }

    /**
     * Cancel a list of scheduling tasks.
     *
     * @param cronIds Task ID list
     */
    public void cancelTaskList(List<Long> cronIds) {
        if (CollectionUtils.isEmpty(cronIds)) {
            return;
        }
        for (Long cronId : cronIds) {
            cancelTask(cronId);
        }
    }

    /**
     * Cancel all scheduling tasks and shutdown the scheduler.
     */
    @PreDestroy
    public void shutdown() {
        // Cancel all tasks
        for (Long cronId : scheduledTasksMap.keySet()) {
            cancelTask(cronId);
        }
        // Shutdown the scheduler
        schedulerPool.getScheduler().shutdown();
    }
}
