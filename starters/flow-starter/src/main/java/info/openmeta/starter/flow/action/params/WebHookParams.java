package info.openmeta.starter.flow.action.params;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Parameters for WebHook.
 */
@Schema(name = "WebHook Params")
@Data
@NoArgsConstructor
public class WebHookParams implements ActionParams {
}
