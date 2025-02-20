package info.openmeta.framework.web.vo;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.Orders;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.time.LocalDate;
import java.util.List;

/**
 * CountParams for /count API.
 */
@Data
@Schema(name = "CountParams")
public class CountParams {

    @Schema(description = "Filters for data to be counted.", type = "array")
    private Filters filters;

    @Schema(description = "Fields for group counts, Return the total count if not specified.", example = "[]")
    private List<String> groupBy;

    @Schema(description = "The field order of the grouped results.", type = "array")
    private Orders orders;

    @Schema(description = "Effective date for timeline model, default is `Today`.")
    private LocalDate effectiveDate;

}
