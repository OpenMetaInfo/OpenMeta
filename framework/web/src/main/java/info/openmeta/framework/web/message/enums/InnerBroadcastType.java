package info.openmeta.framework.web.message.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum InnerBroadcastType {
    RELOAD_METADATA("ReloadMetadata", "Reload Metadata");

    @JsonValue
    private final String type;

    private final String name;

}
