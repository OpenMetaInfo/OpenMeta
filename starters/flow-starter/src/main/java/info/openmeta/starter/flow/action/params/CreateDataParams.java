package info.openmeta.starter.flow.action.params;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * Parameters for creating data.
 * Used to create data based on the model and field value configuration.
 * Field values can be constants, variables (`#{}`), or formulas (`${}`).
 * Example:
 * <p>
 * {
 *     "modelName": "SysModel",
 *     "rowTemplate":  {
 *         "modelName": "#{modelName}",
 *         "modelId": "#{id}",
 *         "appId": "#{appId}",
 *         "fieldName": "effectiveEnd",
 *         "name": "Timeline effectiveEnd",
 *         "fieldType": "DateTime"
 *     }
 * }
 * </p>
 */
@Schema(name = "Create Data Params")
@Data
@NoArgsConstructor
public class CreateDataParams implements ActionParams {

    @Schema(description = "The model name of the data to be created")
    private String model;

    /**
     * The field value configuration of the data to be created.
     * The value can be a constant, a variable, or a calculation formula.
     * Variables are represented by `#{}` and calculation formulas are represented by `${}`.
     */
    @Schema(description = "The field value configuration of the data template")
    private Map<String, Object> rowTemplate;
}
