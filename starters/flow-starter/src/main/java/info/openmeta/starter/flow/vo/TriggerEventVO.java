package info.openmeta.starter.flow.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import jakarta.validation.constraints.NotEmpty;

import java.io.Serializable;
import java.util.Map;

/**
 * Trigger Event VO
 */
@Schema(name = "Trigger Event Body Params")
@Data
public class TriggerEventVO {

    @Schema(description = "Source Model Name")
    @NotBlank(message = "Model Name is required!")
    private String sourceModel;

    @Schema(description = "Row ID")
    private Serializable rowId;

    @Schema(description = "Trigger ID")
    @NotEmpty(message = "Trigger ID is required!")
    private String triggerId;

    @Schema(description = "Event Params")
    private Map<String, Object> eventParams;
}
