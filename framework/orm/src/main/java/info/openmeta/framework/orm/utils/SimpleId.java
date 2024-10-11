package info.openmeta.framework.orm.utils;

import info.openmeta.framework.base.utils.DateUtils;
import lombok.extern.slf4j.Slf4j;

import java.net.NetworkInterface;
import java.net.SocketException;
import java.security.SecureRandom;
import java.time.Instant;
import java.util.Arrays;
import java.util.Enumeration;

/**
 * Generates a trend-increasing ID, but not recommended for use in large concurrent systems.
 * This is a simple 16-digit Long number as a unique ID to compatible with JavaScript Number.MAX_SAFE_INTEGER.
 * The JavaScript Number.MAX_SAFE_INTEGER is 9007199254740991, which is 2^53 - 1.
 * To ensure that, [(current timestamp - start epoch) << 22 | (nodeId << 17) | counter] is less than 2^53 - 1.
 * The start epoch is 2024-01-01T00:00:00Z, so the maximum timestamp is 2092-01-19T03:14:07Z.
 * <p>
 * Consists of:
 *  - 32-bit timestamp in seconds, up to year 2106
 *  - 5 -bit node, up to 32 unique nodes
 *  - 17-bit incrementing counter, up to 131072 unique IDs per second
 */
@Slf4j
public final class SimpleId {

    public static final int NODE_LIMIT = 32;

    // The default epoch shouldn't be changed. The maximum timestamp is 2092-01-19T03:14:07Z.
    public static final long DEFAULT_EPOCH = Instant.parse("2024-01-01T00:00:00Z").getEpochSecond();

    private static final int NODE_BITS = 5;
    private static final int COUNTER_BITS = 17;

    // 2^17 - 1 = 131071
    private static final int MAX_COUNTER_NUMBER = 0x1FFFF;

    private static final int NODE_ID = getNodeIdFromMac();

    // Allow the system time to be adjusted backwards by 10 seconds.
    private static final int ALLOWED_BACKWARD_SECONDS = 10;

    private int counter;

    private long lastTime;

    // Singleton instance
    private static final SimpleId INSTANCE = new SimpleId();

    private SimpleId() {
        counter = getInitialCounter();
        lastTime = DateUtils.getCurrentSeconds();
    }

    public static SimpleId getInstance() {
        return INSTANCE;
    }

    /**
     * Gets the next id
     */
    public synchronized long nextId() {
        long timestamp = DateUtils.getCurrentSeconds();
        if (timestamp < lastTime) {
            if (lastTime - timestamp <= ALLOWED_BACKWARD_SECONDS) {
                log.warn("The system time has been adjusted backwards.");
                timestamp = lastTime;
            } else {
                throw new IllegalStateException("The system time has been adjusted backwards by more than 10 seconds.");
            }
        }
        if (timestamp == lastTime) {
            counter++;
            if (counter == MAX_COUNTER_NUMBER) {
                log.warn("The counter has reached the maximum value of 131072 per second.");
                while (timestamp == lastTime) {
                    timestamp = DateUtils.getCurrentSeconds();
                }
                counter = getInitialCounter();
                this.lastTime = timestamp;
            }
        } else {
            this.counter = getInitialCounter();
            this.lastTime = timestamp;
        }
        return composeId(timestamp, counter);
    }

    /**
     * Get the initial counter value.
     *
     * @return the initial counter value
     */
    public static int getInitialCounter() {
        return new SecureRandom().nextInt(NODE_LIMIT);
    }

    /**
     * Convert the object id to a 16-length long number.
     * The 16-digit long number consists of:
     * - 32-bit timestamp in seconds, up to year 2106
     * - 5 -bit node, up to 32 unique nodes
     * - 17-bit incrementing counter, up to 131072 unique IDs per second
     *
     * @return the 16-length long number
     */
    private long composeId(long timestamp, int counter) {
        long time = timestamp - DEFAULT_EPOCH;
        return (time << (NODE_BITS + COUNTER_BITS)) | ((long) NODE_ID << COUNTER_BITS) | counter;
    }

    /**
     * Get the datetime from the id.
     *
     * @param id the id
     * @return the datetime
     */
    public static String getDatetime(long id) {
        long timestamp = id >> (NODE_BITS + COUNTER_BITS);
        return Instant.ofEpochSecond(timestamp + DEFAULT_EPOCH).toString();
    }

    /**
     * Get the details of the id.
     *
     * @param id the id
     * @return the details of the id
     */
    public String getIdDetails(long id) {
        String dateString = getDatetime(id);
        return "ID: " + id + ", Time: " + dateString + ", Node: " + (id >> COUNTER_BITS & 0x1F)
                + ", Counter: " + (id & MAX_COUNTER_NUMBER);
    }

    /**
     * Get the node ID from the machine's MAC address.
     *
     * @return the node ID
     */
    public static int getNodeIdFromMac() {
        int nodeId;
        byte[] mac = getLocalHardwareAddress();
        if (mac != null) {
            int hashCode = Arrays.hashCode(mac);
            // & 0x7FFFFFFF to ensure the hash value is non-negative
            nodeId = (hashCode & 0x7FFFFFFF) % NODE_LIMIT;
        } else {
            // If the hardware address is not available, use a random number.
            nodeId = new SecureRandom().nextInt(NODE_LIMIT);
        }
        return nodeId;
    }

    /**
     * Get the local hardware address.
     *
     * @return the hardware address
     */
    private static byte[] getLocalHardwareAddress() {
        try {
            Enumeration<NetworkInterface> networkInterfaces = NetworkInterface.getNetworkInterfaces();
            while (networkInterfaces.hasMoreElements()) {
                NetworkInterface ni = networkInterfaces.nextElement();
                if (!ni.isLoopback() && ni.getHardwareAddress() != null) {
                    return ni.getHardwareAddress();
                }
            }
        } catch (SocketException e) {
            return null;
        }
        return null;
    }
}