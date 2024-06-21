package info.openmeta.framework.orm.domain;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.time.LocalDate;

/**
 * Simple aggregation query object, support filters and aggregation functions.
 * Support querying the latest data of each group by using `MAX` + `groupBy`, such as:
 *      AggFunctions: ["MAX", "createdTime", "newestTime"],
 *      GroupBy: ["deptId"],
 */
@Schema(name = "SimpleAggQuery")
@Data
public class SimpleAggQuery {

    @Schema(description = "Filters, support nested filters.")
    private Filters filters;

    @Schema(description = "Aggregation functions, support single or multiple fields aggregation [func, field, alias]")
    private AggFunctions aggFunctions;

    @Schema(description = "Effective date, default is 'today'.")
    private LocalDate effectiveDate;

}
