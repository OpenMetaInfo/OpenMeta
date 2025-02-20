package info.openmeta.framework.web.vo;

import info.openmeta.framework.orm.domain.Filters;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.time.LocalDate;
import java.util.Map;

/**
 * BulkUpdateParams for /updateByFilter API.
 */
@Data
@Schema(name = "BulkUpdateParams")
public class BulkUpdateParams {

    @Schema(description = "Filters for data to be updated.", type = "array")
    private Filters filters;

    @Schema(description = "Field values to be set.", type = "object")
    private Map<String, Object> values;

    @Schema(description = "Effective date for timeline model, default is `Today`.")
    private LocalDate effectiveDate;

}
