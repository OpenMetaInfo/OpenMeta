package info.openmeta.framework.web.controller.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import jakarta.validation.constraints.NotEmpty;

/**
 * Simple VO of a modelName and a fieldName
 */
@Schema(name = "Simple ModelField")
@Data
public class ModelField {

    @Schema(description = "Model name")
    @NotEmpty(message = "The model name cannot be empty!")
    private String model;

    @Schema(description = "Field name")
    @NotEmpty(message = "The field name cannot be empty!")
    private String field;

}
