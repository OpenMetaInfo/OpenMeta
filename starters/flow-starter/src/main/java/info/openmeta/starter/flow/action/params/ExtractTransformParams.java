package info.openmeta.starter.flow.action.params;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Parameters for data extraction and transformation.
 * Extracts data from a collection variable and stores it in a new variable.
 * Example:
 * <p>
 * {
 *     "collectionVariable": "#{GetIds}",
 *     "itemKey": "id"
 * }
 * </p>
 */
@Schema(name = "Extract Transform Params")
@Data
@NoArgsConstructor
public class ExtractTransformParams implements ActionParams {

    @Schema(description = "The variable name of the collection data to be extracted and transformed.")
    private String collectionVariable;

    @Schema(description = "The item key of the source data to be extracted and transformed.")
    private String itemKey;
}
