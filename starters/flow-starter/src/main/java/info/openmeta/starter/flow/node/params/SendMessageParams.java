package info.openmeta.starter.flow.node.params;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Parameters for sending a message.
 */
@Schema(name = "Send Message Params")
@Data
@NoArgsConstructor
public class SendMessageParams implements NodeParams {
}
