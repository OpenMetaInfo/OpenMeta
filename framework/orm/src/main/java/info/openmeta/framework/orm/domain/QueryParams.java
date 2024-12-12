package info.openmeta.framework.orm.domain;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.orm.enums.ConvertType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

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
@Schema(name = "QueryParams")
public class QueryParams {

    @Schema(description = "Fields list to get, empty means all fields of the model.", example = "[\"id\", \"name\"]")
    private List<String> fields;

    private Filters filters;

    private Orders orders;

    private AggFunctions aggFunctions;

    @Schema(description = "Page number, start from 1, default 1.", example = "1")
    private Integer pageNumber;

    @Schema(description = "Page size, or limit size for searchList, default 50.", example = "50")
    private Integer pageSize;

    @Schema(description = "Fields to group by, empty means no grouping.", example = "[]")
    private List<String> groupBy;

    @Schema(description = "Pivot split field list.", example = "[]")
    private List<String> splitBy;

    @Schema(description = "Whether to return the summary result of numeric fields, default no summary.")
    private Boolean summary;

    @Schema(description = "Effective date, default is `Today`.")
    private LocalDate effectiveDate;

    @Schema(description = "Sub queries for relational fields: {fieldName: SubQuery}", example = "{}")
    private Map<String, SubQuery> subQueries;


    /**
     * Convert QueryParams to FlexQuery.
     *
     * @param queryParams QueryParams
     * @return FlexQuery
     */
    static public FlexQuery convertParamsToFlexQuery(QueryParams queryParams) {
        if (queryParams == null) {
            queryParams = new QueryParams();
        }
        ContextHolder.getContext().setEffectiveDate(queryParams.getEffectiveDate());
        FlexQuery flexQuery = new FlexQuery(queryParams.getFilters(), queryParams.getOrders());
        flexQuery.setFields(queryParams.getFields());
        flexQuery.setConvertType(ConvertType.REFERENCE);
        flexQuery.setGroupBy(queryParams.getGroupBy());
        // Set AggFunction parameters
        flexQuery.setAggFunctions(queryParams.getAggFunctions());
        // Set SubQuery parameters
        flexQuery.expandSubQueries(queryParams.getSubQueries());
        return flexQuery;
    }
}
