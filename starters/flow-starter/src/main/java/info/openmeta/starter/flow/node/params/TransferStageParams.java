package info.openmeta.starter.flow.node.params;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Parameters for transferring stage.
 */
@Schema(name = "Transfer Stage Params")
@Data
@NoArgsConstructor
public class TransferStageParams implements NodeParams {
}
