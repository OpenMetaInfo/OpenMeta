package info.openmeta.starter.flow.node.params;

import info.openmeta.framework.orm.enums.FieldType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Parameters for computing data.
 * Used to calculate a variable's value based on an expression.
 * Example:
 * <p>
 * {
 *     "expression": "a + b",
 *     "valueType": "Integer"
 * }
 * </p>
 */
@Schema(name = "Compute Data Params")
@Data
@NoArgsConstructor
public class ComputeDataParams implements NodeParams {

    @Schema(description = "Compute Expression")
    private String expression;

    @Schema(description = "Value Type of the Compute Result, refer to FieldType")
    private FieldType valueType;
}
