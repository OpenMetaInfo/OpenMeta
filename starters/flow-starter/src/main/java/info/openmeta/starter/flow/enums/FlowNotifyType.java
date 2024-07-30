package info.openmeta.starter.flow.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Flow Notify Type
 */
@Getter
@AllArgsConstructor
public enum FlowNotifyType {
    TASK_NOTIFY("TaskNotify"),
    EXCEPTION_NOTIFY("ExceptionNotify"),
    STATUS_CHANGED("StatusChanged"),
    REMIND_NOTIFY("RemindNotify"),
    MESSAGE_NOTIFY("MessageNotify");

    @JsonValue
    private final String type;
}
