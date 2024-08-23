package info.openmeta.starter.flow.action.params;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Parameters for querying AI using robot ID, conversation ID, and query text.
 * The query text supports string interpolation `#{var}`.
 */
@Schema(name = "Query AI Params")
@Data
@NoArgsConstructor
public class QueryAiParams implements ActionParams {

    @Schema(description = "Robot ID")
    private Long robotId;

    @Schema(description = "Conversation ID")
    private Long conversationId;

    @Schema(description = "Query text, support string interpolation #{var}")
    private String query;

}
