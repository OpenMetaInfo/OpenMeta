package info.openmeta.starter.flow.node.params;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Parameters for BranchGateway node.
 */
@Schema(name = "BranchGateway Params")
@Data
@NoArgsConstructor
public class BranchGatewayParams implements NodeParams {

    @Schema(description = "Whether the gateway is a serial gateway, default is parallel gateway.")
    private Boolean serialGateway;

}
