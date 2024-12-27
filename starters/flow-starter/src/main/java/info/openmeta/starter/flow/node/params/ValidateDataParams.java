package info.openmeta.starter.flow.node.params;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Parameters for data validation.
 */
@Schema(name = "Validate Data Params")
@Data
@NoArgsConstructor
public class ValidateDataParams implements NodeParams {

    @Schema(description = "Validation expression")
    private String expression;

    @Schema(description = """
            The exception message when the validation fails.
            Supports string interpolation using environment variables,
            such as "Department ID #{deptId} does not exist!"
            """)
    private String exceptionMsg;
}
