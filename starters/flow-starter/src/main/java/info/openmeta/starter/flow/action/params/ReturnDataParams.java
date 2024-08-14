package info.openmeta.starter.flow.action.params;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * Parameters for returning data.
 * The value supports constants, variables, and calculation formulas,
 * where variables are represented by `#{}` and calculation formulas are represented by `${}`.
 * Example:
 * <p>
 * {
 *     "dataTemplate":  {
 *         "parentId": "#{parentId}",
 *         "name": "#{deptName}",
 *         "ownId": "#{ownId}"
 *     }
 * }
 * </p>
 */
@Schema(name = "Return Data Params")
@Data
@NoArgsConstructor
public class ReturnDataParams implements ActionParams {

    @Schema(description = """
            The key-value structure configuration of the returned data.
            The value can be a constant, a variable, or a calculation formula.
            Variables are represented by `#{}` and calculation formulas are represented by `${}`.
            """)
    private Map<String, Object> dataTemplate;
}
