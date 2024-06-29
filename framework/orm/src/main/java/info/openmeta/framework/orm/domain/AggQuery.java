package info.openmeta.framework.orm.domain;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Aggregation query object, support aggregation functions and sub queries.
 * Support querying the latest data of each group by using `MAX` + `groupBy`, such as:
 *      AggFunctions: ["MAX", "createdTime", "newestTime"],
 *      GroupBy: ["deptId"],
 */
@Schema(name = "AggQuery")
@Data
public class AggQuery {

    @Schema(description = "Fields to get, empty means all fields.", example = "[\"id\", \"name\"]")
    private Set<String> fields;

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

    @Schema(description = "Whether to return the summary result of numeric fields, empty means not return.")
    private Boolean summary;

    @Schema(description = "Effective date, default is 'today'.")
    private LocalDate effectiveDate;

    @Schema(description = "Sub queries for relational fields: {fieldName: SubQuery}", example = "{}")
    private Map<String, SubQuery> subQueries;

}
