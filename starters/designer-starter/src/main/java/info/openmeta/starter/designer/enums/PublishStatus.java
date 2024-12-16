package info.openmeta.starter.designer.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum PublishStatus {
    UNPUBLISH("Unpublish", "Unpublish"),
    PUBLISHING("Publishing", "Publishing"),
    PUBLISHED("Published", "Published"),
    FAILURE("Failure", "Failure");

    @JsonValue
    private final String name;

    private final String description;
}
