package info.openmeta.starter.cron.scheduler;

import info.openmeta.framework.base.exception.SystemException;
import jakarta.annotation.PreDestroy;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * Scheduler leader election based on Redis.
 * The leader node renews, and the candidate node tries to acquire the lease.
 * Suitable for scheduling task, which can tolerate repeated scheduling in a short time under
 * extreme circumstances, and a short time when no leader is generated during the lease period.
 */
@Slf4j
@Component
public class LeaderElectionRunner implements CommandLineRunner {
    private static final String LEADER_KEY = "cron:scheduler:leader";
    private static final String LEADER_UUID = UUID.randomUUID().toString();

    /** Leader lease duration in seconds */
    private static final int LEADER_EXPIRE_SECONDS = 60;
    /** Leader renewal interval, half of the lease duration, in seconds */
    private static final int LEADER_RENEW_INTERVAL = LEADER_EXPIRE_SECONDS / 2;
    /** Leader renewal executor, single thread */
    private final ScheduledExecutorService leaderRenewalExecutor = Executors.newScheduledThreadPool(1);

    @Autowired
    private StringRedisTemplate redisTemplate;

    @Autowired
    private CronScheduler cronScheduler;

    private boolean isLeader = false;

    /**
     * Try to acquire the Leader lock, renew once every half of the lease duration.
     */
    @Override
    public void run(String... args) {
        Runnable leaderElectionTask = () -> {
            try {
                if (tryAcquireLeaderLease()) {
                    setLeader(true);
                } else if (isLeader) {
                    renewLeaderLease();
                }
            } catch (Exception e) {
                log.error("Cron job scheduler Leader election failed, an exception occurred:", e);
                terminateLeadership();
            }
        };
        // Try to acquire the lock or renew once during each lease period
        leaderRenewalExecutor.scheduleAtFixedRate(leaderElectionTask, 0, LEADER_RENEW_INTERVAL, TimeUnit.SECONDS);
    }

    /**
     * Set Leader status
     *
     * @param isLeader Leader status
     */
    public void setLeader(boolean isLeader) {
        if (this.isLeader != isLeader) {
            this.isLeader = isLeader;
            onLeaderChange(isLeader);
        }
    }

    /**
     * Leader status change triggers scheduler change
     *
     * @param isLeader Leader status
     */
    public void onLeaderChange(boolean isLeader) {
        if (isLeader) {
            log.info("Leader elected, starting the scheduler.");
            try {
                // Start all active cron jobs
                cronScheduler.start();
            } catch (Exception e) {
                throw new SystemException("Scheduler startup failed: {0}", e.getMessage(), e);
            }
        } else {
            // Stop the scheduler and cancel all scheduling tasks when no longer the leader
            cronScheduler.shutdown();
        }
    }

    /**
     * Try to acquire the Leader lock with the lease duration.
     *
     * @return true if the Leader lock is acquired
     */
    private boolean tryAcquireLeaderLease() {
        Boolean acquired = redisTemplate.opsForValue().setIfAbsent(LEADER_KEY, LEADER_UUID, LEADER_EXPIRE_SECONDS, TimeUnit.SECONDS);
        return Boolean.TRUE.equals(acquired);
    }

    /**
     * Renew the `Leader` lease.
     * Check if the current `Leader` is itself before renewing
     */
    private void renewLeaderLease() {
        String currentLeaderUUID = redisTemplate.opsForValue().get(LEADER_KEY);
        if (LEADER_UUID.equals(currentLeaderUUID)) {
            // If the `Leader` is itself, renew the lease
            redisTemplate.expire(LEADER_KEY, LEADER_EXPIRE_SECONDS, TimeUnit.SECONDS);
        } else {
            // If the `Leader` is not itself, release the lease
            terminateLeadership();
        }
    }

    /**
     * Terminate the `Leader` lease
     */
    private void terminateLeadership() {
        String currentLeaderUUID = redisTemplate.opsForValue().get(LEADER_KEY);
        if (LEADER_UUID.equals(currentLeaderUUID)) {
            redisTemplate.delete(LEADER_KEY);
        }
        setLeader(false);
    }

    /**
     * Quit from the `Leader` election
     */
    @PreDestroy
    public void quitElection() {
        leaderRenewalExecutor.shutdown();
        terminateLeadership();
    }
}
