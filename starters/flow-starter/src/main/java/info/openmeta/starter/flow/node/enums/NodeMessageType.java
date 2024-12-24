package info.openmeta.starter.flow.node.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Node Message Type: SMS, Email, Inner Message, IM Message
 */
@Getter
@AllArgsConstructor
public enum NodeMessageType {
    SMS("SMS"),
    EMAIL("Email"),
    INNER_MSG("InnerMsg"),
    IM_MSG("IMMsg");

    @JsonValue
    private final String type;
}
