package info.openmeta.framework.web.vo;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.enums.ConvertType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.apache.commons.lang3.StringUtils;

import java.time.LocalDate;
import java.util.List;

/**
 * SearchNameParams
 * The parameters for searching by name.
 * The value is used to match the displayName field by default. Or specify the matchField.
 */
@Data
@Schema(name = "SearchNameParams")
public class SearchNameParams {

    @Schema(description = "Optionally specify the matching field. Default is `searchName` field configured in the model.", example = "searchName")
    private String matchField = ModelConstant.SEARCH_NAME;

    @Schema(description = "The operator to use for the search. Default is CONTAINS.", example = "CONTAINS")
    private Operator operator = Operator.CONTAINS;

    @Schema(description = "The value to match using CONTAINS.", example = "Tom")
    private String value;

    @Schema(description = "Additional fields to fetch, in addition to displayName.", example = "[\"type\"]")
    private List<String> additionalFields;

    @Schema(description = "Context filters for the search. From the relational field or form context.")
    private Filters filters;

    @Schema(description = "Ordering for the search results.")
    private Orders orders;

    @Schema(description = "Limit size for search, default 10.", example = "10")
    private Integer limit = BaseConstant.DEFAULT_NAME_LIST_SIZE;

    @Schema(description = "Effective date, default is `Today`.")
    private LocalDate effectiveDate;

    /**
     * Convert QueryParams to FlexQuery.
     *
     * @param searchNameParams QueryParams
     * @return FlexQuery
     */
    static public FlexQuery convertParamsToFlexQuery(SearchNameParams searchNameParams) {
        if (searchNameParams == null) {
            searchNameParams = new SearchNameParams();
        }
        Filters filters = searchNameParams.getFilters();
        if (StringUtils.isNotBlank(searchNameParams.getValue())) {
            // Construct the match filters.
            String matchField = StringUtils.isBlank(searchNameParams.getMatchField()) ? ModelConstant.SEARCH_NAME : searchNameParams.getMatchField();
            Operator operator = searchNameParams.getOperator() == null ? Operator.CONTAINS : searchNameParams.getOperator();
            Filters matchFilters = Filters.of(matchField, operator, searchNameParams.getValue());
            filters = Filters.and(filters, matchFilters);
        }
        FlexQuery flexQuery = new FlexQuery(filters, searchNameParams.getOrders());
        flexQuery.setFields(searchNameParams.getAdditionalFields());
        // Set the convert type to REFERENCE.
        flexQuery.setConvertType(ConvertType.REFERENCE);
        // Default limitSize for searchName.
        Integer limitSize = searchNameParams.getLimit();
        limitSize = limitSize == null || limitSize < 1 ? BaseConstant.DEFAULT_NAME_LIST_SIZE : limitSize;
        Assert.isTrue(limitSize <= BaseConstant.MAX_BATCH_SIZE,
                "API `searchName` cannot exceed the maximum limit of {0}.", BaseConstant.MAX_BATCH_SIZE);
        flexQuery.setLimitSize(limitSize);
        ContextHolder.getContext().setEffectiveDate(searchNameParams.getEffectiveDate());
        return flexQuery;
    }
}
