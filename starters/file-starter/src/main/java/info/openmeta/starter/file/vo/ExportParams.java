package info.openmeta.starter.file.vo;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.orm.domain.AggFunctions;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.enums.ConvertType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.time.LocalDate;
import java.util.List;

/**
 * General query parameters object specifically designed for API request parameter transmission.
 * This class encapsulates all necessary parameters for performing various types of API queries,
 * including aggregation functions, sorting, and sub-queries.
 * <p>
 * It is intended solely for API endpoint consumption and is not suitable for internal service logic.
 * <p>
 * Support querying the latest data of each group by using `MAX` + `groupBy`, such as
 *      AggFunctions: ["MAX", "createdTime", "newestTime"],
 *      GroupBy: ["deptId"],
 */
@Data
@Schema(name = "ExportParams")
public class ExportParams {

    @Schema(description = "Fields list to get, empty means all fields of the model.", example = "[\"id\", \"name\"]")
    private List<String> fields;

    private Filters filters;

    private Orders orders;

    private AggFunctions aggFunctions;

    @Schema(description = "Limit size for searchList, default 50.", example = "50")
    private Integer limit;

    @Schema(description = "Fields to group by, empty means no grouping.", example = "[]")
    private List<String> groupBy;

    @Schema(description = "Effective date, default is `Today`.")
    private LocalDate effectiveDate;

    /**
     * Convert ExportParams to FlexQuery.
     *
     * @param exportParams ExportParams
     * @return FlexQuery
     */
    static public FlexQuery convertParamsToFlexQuery(ExportParams exportParams) {
        if (exportParams == null) {
            exportParams = new ExportParams();
        }
        FlexQuery flexQuery = new FlexQuery(exportParams.getFilters(), exportParams.getOrders());
        flexQuery.setFields(exportParams.getFields());
        // Set the convert type to DISPLAY
        flexQuery.setConvertType(ConvertType.DISPLAY);
        flexQuery.setGroupBy(exportParams.getGroupBy());
        // Set AggFunction parameters
        flexQuery.setAggFunctions(exportParams.getAggFunctions());
        ContextHolder.getContext().setEffectiveDate(exportParams.getEffectiveDate());
        return flexQuery;
    }
}
