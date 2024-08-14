package info.openmeta.starter.flow.node;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * Parameters for dataset loop processing:
 *      for {loopItemNaming} in dataSetParam
 */
@Data
@Schema(name = "LoopByDatasetParams")
public class LoopByDatasetParams {
    @Schema(description = "Dataset parameter variable name")
    private String dataSetParam;

    @Schema(description = "Loop item naming")
    private String loopItemNaming;
}
