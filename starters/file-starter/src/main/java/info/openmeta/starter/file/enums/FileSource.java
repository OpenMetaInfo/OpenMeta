package info.openmeta.starter.file.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum FileSource {
    DOWNLOAD("Download"),
    UPLOAD("Upload"),
    ;

    @JsonValue
    private final String code;
}
