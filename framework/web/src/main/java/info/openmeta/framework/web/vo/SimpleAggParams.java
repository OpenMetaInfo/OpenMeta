package info.openmeta.framework.web.vo;

import info.openmeta.framework.orm.domain.AggFunctions;
import info.openmeta.framework.orm.domain.Filters;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.time.LocalDate;

/**
 * Simple aggregation query params object, support filters and aggregation functions.
 * Support querying the latest data of each group by using `MAX` + `groupBy`, such as:
 *      AggFunctions: ["MAX", "createdTime", "newestTime"],
 *      GroupBy: ["deptId"],
 */
@Schema(name = "SimpleAggParams")
@Data
public class SimpleAggParams {

    @Schema(description = "Filters, support nested filters.")
    private Filters filters;

    @Schema(description = "Aggregation functions, support single or multiple fields aggregation [func, field, alias]")
    private AggFunctions aggFunctions;

    @Schema(description = "Effective date, default is `Today`.")
    private LocalDate effectiveDate;

}
