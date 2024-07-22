package info.openmeta.framework.web.controller.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

/**
 * Simple VO of a modelName and a fieldName
 */
@Schema(name = "Simple ModelField")
@Data
public class ModelField {

    @Schema(description = "Model name")
    @NotBlank(message = "The model name cannot be empty!")
    private String model;

    @Schema(description = "Field name")
    @NotBlank(message = "The field name cannot be empty!")
    private String field;

}
