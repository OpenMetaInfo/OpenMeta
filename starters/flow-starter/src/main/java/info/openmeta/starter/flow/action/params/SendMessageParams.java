package info.openmeta.starter.flow.action.params;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Parameters for sending a message.
 */
@Schema(name = "Send Message Params")
@Data
@NoArgsConstructor
public class SendMessageParams implements ActionParams {
}
