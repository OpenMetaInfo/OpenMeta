package info.openmeta.starter.flow.action.params;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * Parameters for an async task.
 * Executes a processor asynchronously, possibly in a different thread or service.
 * Example:
 * <p>
 * {
 *     "asyncTaskCode": "CleanDataLog",
 *     "dataTemplate": {
 *         "id": "#{GetData.id}",
 *         "reason": "AutoClean",
 *         "description": "Auto Clean"
 *     }
 * }
 * </p>
 */
@Schema(name = "Async Task Params")
@Data
@NoArgsConstructor
public class AsyncTaskParams implements ActionParams {

    @Schema(description = "The processor code of the async task")
    private String asyncTaskCode;

    @Schema(description = """
            The params for the async task. The value can be a constant, a variable, or a calculation formula.
            Variables are represented by `#{}` and calculation formulas are represented by `${}`.
            """)
    private Map<String, Object> dataTemplate;
}
