package info.openmeta.starter.flow;

import info.openmeta.framework.orm.utils.MapUtils;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Map;

/**
 * Flow environment variables, which are shared by all flow nodes.
 */
public class FlowEnv {

    private static final String NOW = "NOW";
    private static final String TODAY = "TODAY";
    private static final String YESTERDAY = "YESTERDAY";

    public static Map<String, Object> getEnv() {
        return MapUtils.strObj()
                .put(NOW, LocalDateTime.now())
                .put(TODAY, LocalDate.now())
                .put(YESTERDAY, LocalDate.now().minusDays(1))
                .build();
    }
}
