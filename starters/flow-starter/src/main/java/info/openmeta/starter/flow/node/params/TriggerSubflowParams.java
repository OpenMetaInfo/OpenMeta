package info.openmeta.starter.flow.node.params;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * Parameters for triggering a subflow.
 */
@Schema(name = "Trigger Subflow Params")
@Data
@NoArgsConstructor
public class TriggerSubflowParams implements NodeParams {

    @Schema(description = "Subflow trigger ID")
    private String subflowTriggerId;

    @Schema(description = """
            The key-value structure configuration of the subflow data.
            The value can be a constant, a variable, or a calculation formula.
            Variables are represented by `#{}` and calculation formulas are represented by `${}`.
            """)
    private Map<String, Object> dataTemplate;

}
