package info.openmeta.starter.metadata.controller.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

/**
 * Simple VO of modelName and view ID
 */
@Schema(name = "ModelViewVO")
@Data
public class ModelViewVO {

    @Schema(description = "Model name")
    @NotBlank(message = "The model name cannot be empty!")
    private String model;

    @Schema(description = "View ID")
    @NotNull(message = "The view ID cannot be null!")
    private Long viewId;

}
