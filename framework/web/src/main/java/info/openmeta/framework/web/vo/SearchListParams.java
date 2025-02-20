package info.openmeta.framework.web.vo;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.domain.*;
import info.openmeta.framework.orm.enums.ConvertType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.util.CollectionUtils;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

/**
 * SearchListParams for /searchList API.
 * <p>
 * It is intended solely for API endpoint consumption and is not suitable for internal service logic.
 * <p>
 * Support querying the latest data of each group by using `MAX` + `groupBy`, such as
 *      AggFunctions: ["MAX", "createdTime", "newestTime"],
 *      GroupBy: ["deptId"],
 */
@Data
@Schema(name = "SearchListParams")
public class SearchListParams {

    @Schema(description = "Fields list to get, empty means all fields of the model.", example = "[\"id\", \"name\"]")
    private List<String> fields;

    private Filters filters;

    private Orders orders;

    private AggFunctions aggFunctions;

    @Schema(description = "Limit size for searchList, default 50.", example = "50")
    private Integer limit;

    @Schema(description = "Fields to group by, empty means no grouping.", example = "[]")
    private List<String> groupBy;

    @Schema(description = "Pivot split field list.", example = "[]")
    private List<String> splitBy;

    @Schema(description = "Effective date, default is `Today`.")
    private LocalDate effectiveDate;

    @Schema(description = "Sub queries for relational fields: {fieldName: SubQuery}", example = "{}")
    private Map<String, SubQuery> subQueries;

    /**
     * Convert SearchListParams to FlexQuery.
     *
     * @param searchListParams SearchListParams
     * @return FlexQuery
     */
    static public FlexQuery convertParamsToFlexQuery(SearchListParams searchListParams) {
        if (searchListParams == null) {
            searchListParams = new SearchListParams();
        }
        FlexQuery flexQuery = new FlexQuery(searchListParams.getFilters(), searchListParams.getOrders());
        flexQuery.setFields(searchListParams.getFields());
        flexQuery.setConvertType(ConvertType.REFERENCE);
        flexQuery.setGroupBy(searchListParams.getGroupBy());
        // Set AggFunction parameters
        flexQuery.setAggFunctions(searchListParams.getAggFunctions());
        // Default limitSize for searchList.
        Integer limitSize = searchListParams.getLimit();
        limitSize = limitSize == null || limitSize < 1 ? BaseConstant.DEFAULT_PAGE_SIZE : limitSize;
        Assert.isTrue(limitSize <= BaseConstant.MAX_BATCH_SIZE,
                "API `searchList` cannot exceed the maximum limit of {0}.", BaseConstant.MAX_BATCH_SIZE);
        flexQuery.setLimitSize(limitSize);
        // Set SubQuery parameters
        if (!CollectionUtils.isEmpty(searchListParams.getSubQueries())) {
            SubQueries subQueries = new SubQueries();
            subQueries.setQueryMap(searchListParams.getSubQueries());
            flexQuery.setSubQueries(subQueries);
        }
        ContextHolder.getContext().setEffectiveDate(searchListParams.getEffectiveDate());
        return flexQuery;
    }
}
