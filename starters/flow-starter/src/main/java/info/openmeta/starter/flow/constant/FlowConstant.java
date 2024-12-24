package info.openmeta.starter.flow.constant;

import java.util.Set;

/**
 * Flow Constant
 */
public interface FlowConstant {

    // The key of the trigger data record in the node context environment variable
    String TRIGGER_PARAMS = "TriggerParams";
    String TRIGGER_ROW_ID = "TriggerRowId";

    /**
     * When it is not a verification flow, the event trigger model to be excluded,
     * to avoid creating data of these models during the creation of Flow causing a dead loop.
     */
    Set<String> EXCLUDE_TRIGGER_MODELS = Set.of("FlowInstance", "FlowEvent");
}
