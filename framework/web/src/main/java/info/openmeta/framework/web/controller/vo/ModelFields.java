package info.openmeta.framework.web.controller.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import jakarta.validation.constraints.NotEmpty;
import java.util.Set;

/**
 * Simple VO of a modelName and multiple fieldNames
 */
@Schema(name = "Simple ModelFields")
@Data
public class ModelFields {

    @Schema(description = "Model name")
    @NotEmpty(message = "The model name cannot be empty!")
    private String model;

    @Schema(description = "Field names. If it is empty, all fields meet the conditions by default.")
    private Set<String> fields;

}
