package info.openmeta.starter.flow.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Flow Layout Type
 */
@Getter
@AllArgsConstructor
public enum FlowLayoutType {
    VERTICAL_AUTOMATIC("Vertical Automatic"),
    HORIZONTAL_AUTOMATIC("Horizontal Automatic"),
    MANUAL_LAYOUT("Manual Layout"),
    ;

    @JsonValue
    private final String type;
}
