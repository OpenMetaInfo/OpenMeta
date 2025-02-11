package info.openmeta.framework.web.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.util.Set;

/**
 * Simple VO of a modelName and multiple fieldNames
 */
@Schema(name = "Simple ModelFields")
@Data
public class ModelFields {

    @Schema(description = "Model name")
    @NotBlank(message = "The model name cannot be empty!")
    private String model;

    @Schema(description = "Field names. If it is empty, all fields meet the conditions by default.")
    private Set<String> fields;

}
