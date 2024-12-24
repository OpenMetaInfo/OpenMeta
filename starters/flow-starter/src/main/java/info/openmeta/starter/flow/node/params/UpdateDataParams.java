package info.openmeta.starter.flow.node.params;

import info.openmeta.framework.orm.domain.Filters;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * Parameters for updating data.
 * Update data based on the specified model, primary key or filters, and row template.
 * The value supports constants, variables, and calculation formulas,
 * where variables are represented by `#{}` and calculation formulas are represented by `${}`.
 * Example:
 * <p>
 * {
 *     "modelName": "SysModel",
 *     "pkVariable": "#{deptId}",
 *     "filters": ["code", "=", "#{deptCode}"],
 *     "rowTemplate":  {
 *         "parentId": "#{parentId}",
 *         "name": "#{deptName}",
 *         "ownId": "#{ownId}"
 *     }
 * }
 * </p>
 */
@Schema(name = "Update Data Params")
@Data
@NoArgsConstructor
public class UpdateDataParams implements NodeParams {

    @Schema(description = "The model of the data to be updated")
    private String modelName;

    @Schema(description = """
            The primary key variable name of the data to be updated,
            supports single value, multi-value variables #{var}.""")
    private String pkVariable;

    @Schema(description = """
            The filters of the data to be updated, value supports constants, variables #{},
            calculation formulas ${}, reserved field names @{}.""")
    private Filters filters;

    @Schema(description = """
            The key-value structure configuration of the updated data.
            The value can be a constant, a variable, or a calculation formula.
            Variables are represented by `#{}` and calculation formulas are represented by `${}`.
            """)
    private Map<String, Object> rowTemplate;
}
