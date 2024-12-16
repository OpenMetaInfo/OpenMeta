package info.openmeta.starter.designer.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum CodeFileType {
    ENTITY("entity", "Entity Class"),
    SERVICE("service", "Service"),
    SERVICE_IMPL("serviceImpl", "Service Implementation Class"),
    CONTROLLER("controller", "Controller");

    @JsonValue
    private final String code;
    private final String name;

}
