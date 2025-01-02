package info.openmeta.starter.flow.node.params;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Parameters for generating a report based on the configuration.
 */
@Schema(name = "Generate Report Params")
@Data
@NoArgsConstructor
public class GenerateReportParams implements NodeParams {

}
