package info.openmeta.starter.flow.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import jakarta.validation.constraints.NotEmpty;
import java.util.Map;

/**
 * Trigger Event VO
 */
@Schema(name = "Trigger Event Body Params")
@Data
public class TriggerEventVO {

    @Schema(description = "Model Name")
    @NotBlank(message = "Model Name is required!")
    private String model;

    @Schema(description = "Row ID")
    private Long rowId;

    @Schema(description = "Trigger Code")
    @NotEmpty(message = "Trigger Code is required!")
    private String triggerCode;

    @Schema(description = "Event Params")
    private Map<String, Object> eventParams;
}
