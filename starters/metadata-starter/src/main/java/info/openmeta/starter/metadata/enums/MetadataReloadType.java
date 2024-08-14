package info.openmeta.starter.metadata.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum MetadataReloadType {
    RELOAD_MODEL_MANAGER("ReloadModelManager", "Reload ModelManager"),
    RELOAD_OPTION_MANAGER("ReloadOptionManager", "Reload OptionManager");

    @JsonValue
    private final String type;

    private final String name;

}
