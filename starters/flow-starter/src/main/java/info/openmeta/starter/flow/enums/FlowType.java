package info.openmeta.starter.flow.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Flow Type
 */
@Getter
@AllArgsConstructor
public enum FlowType {
    AUTOMATED_FLOW("AutomatedFlow", "Automated Flow", ""),
    FORM_FLOW("FormFlow", "Form Flow", "Form data submission flow"),
    VALIDATION_FLOW("ValidationFlow", "Validation Flow",
            "Triggered before transaction commit, used for data validation. Rollback transaction if validation failed."),
    ONCHANGE_FLOW("OnchangeFlow", "Onchange Flow",
            "Triggered by field value change, used for field value calculation. No transaction commit."),
    AGENT_FLOW("AgentFlow", "Agent Flow", "Used for agent task execution"),;

    @JsonValue
    private final String type;
    private final String name;
    private final String description;
}
