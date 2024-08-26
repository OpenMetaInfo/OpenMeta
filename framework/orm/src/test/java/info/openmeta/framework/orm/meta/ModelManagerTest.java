package info.openmeta.framework.orm.meta;

import info.openmeta.framework.orm.compute.ComputeUtils;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
class ModelManagerTest {

    @Test
    void initComputedFields() {
        String formula = "if seq != \"6\" { \"17\" } else { \"99\" }";
        List<String> dependentFields = ComputeUtils.compile(formula).getVariableFullNames();
        Assertions.assertNotNull(dependentFields);
        Map<String, Object> env = new HashMap<>();
        env.put("seq", "5");
        Object result = ComputeUtils.compile(formula).execute(env);
        log.info(result.toString());
    }
}