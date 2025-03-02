package info.openmeta.starter.flow.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Flow Trigger Event Type
 */
@Getter
@AllArgsConstructor
public enum TriggerEventType {
    CREATE_EVENT("CreateEvent", "Create Event"),
    UPDATE_EVENT("UpdateEvent", "Update Event"),
    CREATE_OR_UPDATE("CreateOrUpdate", "Create Or Update Event"),
    DELETE_EVENT("DeleteEvent", "Delete Event"),
    CHANGED_EVENT("ChangedEvent", "Changed Event(C/U/D)"),
    ONCHANGE_EVENT("OnchangeEvent", "Onchange Event"),
    API_EVENT("ApiEvent", "API Event"),
    CRON_EVENT("CronEvent", "Cron Event"),
    SUBFLOW_EVENT("SubflowEvent", "Subflow Event");

    @JsonValue
    private final String type;
    private final String name;
}
