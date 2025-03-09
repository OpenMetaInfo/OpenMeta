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

    @Schema(description = "The message to send.", required = true)
    private String message;

    @Schema(description = "The recipient of the message.", required = true)
    private String recipient;

    @Schema(description = "The sender of the message.", required = true)
    private String sender;
}
