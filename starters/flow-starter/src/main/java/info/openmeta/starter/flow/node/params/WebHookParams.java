package info.openmeta.starter.flow.node.params;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Parameters for WebHook.
 */
@Schema(name = "WebHook Params")
@Data
@NoArgsConstructor
public class WebHookParams implements NodeParams {
}
