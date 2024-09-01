package info.openmeta.framework.orm.domain;

import info.openmeta.framework.base.utils.SFunction;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.orm.utils.BeanTool;
import info.openmeta.framework.orm.utils.LambdaUtils;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Flexible query building utility class, used to build query conditions for model data.
 * Including the following parameters:
 *      fields, groupBy, filters, orders, convertType, pageNumber, pageSize, distinct, disabled
 * The return value can be divided into four scenarios:
 *      1. Map list: fields + groupBy + [filters, orders] + convertType + disabled
 *      2. Map paginated list: fields + groupBy + [filters, orders] + convertType + disabled + [pageNumber, pageSize]
 *      3. Object list: [filters, orders] + disabled
 *      4. Object paginated list: [filters, orders] + disabled + [pageNumber, pageSize]
 * Among them, groupBy is an ordered field list. When the groupBy parameter has a value,
 * the `count(*) as count` return value will be automatically added,
 * and the `sum()` aggregation calculation will be automatically performed on the numerical fields.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class FlexQuery {

    // Specifies a list of fields to read
    private Set<String> fields = new HashSet<>();
    // Filters conditions can be empty
    private Filters filters;
    // `orderBy` rule, using the model default configuration when empty
    private Orders orders;

    // A limit on the number of returns for list queries (non-paged queries)
    private Integer limitSize;

    // Convert the return value type
    private ConvertType convertType = ConvertType.DEFAULT;

    // In special cases, such as requests based on OneToMany fields,
    // the ManyToOne of the associated model only needs to obtain the ID for GroupBy
    private String keepIdField;

    // Aggregation query, including aggFunctions, groupBy, splitBy
    private boolean aggregate;

    // GroupBy field list
    private List<String> groupBy = new ArrayList<>();
    // SplitBy field list in PivotTable
    private List<String> splitBy = new ArrayList<>();

    // Whether to add a summary row for numerical fields to the result
    private boolean summary;

    // Whether to remove duplicate rows when querying data from the database
    private boolean distinct;

    // Whether to search  across time periods, used to query in all timeline data and modify historical timeline rows.
    private boolean acrossTimeline;

    // Aggregation function queries
    private AggFunctions aggFunctions;

    // SubQuery parameters: field name - subQuery
    private Map<String, SubQuery> subQueries = new HashMap<>(0);

    /**
     * Construct a FlexQuery object based on Fields.
     *
     * @param fields List of fields to read
     */
    public FlexQuery(Collection<String> fields) {
        if (!CollectionUtils.isEmpty(fields)) {
            this.fields = new HashSet<>(fields);
        }
    }

    /**
     * Set the field list based on the lambda method reference
     * @param lambdaFields Field lambda method reference
     * @return Field name list
     */
    @SafeVarargs
    public static <T, R> FlexQuery ofFields(SFunction<T, R>... lambdaFields) {
        List<String> fields = Arrays.stream(lambdaFields).map(LambdaUtils::getAttributeName).collect(Collectors.toList());
        return new FlexQuery(fields);
    }

    /**
     * Construct a FlexQuery object based on Fields and Filters.
     *
     * @param fields  List of fields to read
     * @param filters Filter conditions
     */
    public FlexQuery(Collection<String> fields, Filters filters) {
        if (!CollectionUtils.isEmpty(fields)) {
            this.fields = new HashSet<>(fields);
        }
        this.filters = filters;
    }

    /**
     * Construct a FlexQuery object based on Fields, Filters, and Orders.
     *
     * @param fields  List of fields to read
     * @param filters Filter conditions
     * @param orders  Sort rules
     */
    public FlexQuery(Collection<String> fields, Filters filters, Orders orders) {
        if (!CollectionUtils.isEmpty(fields)) {
            this.fields = new HashSet<>(fields);
        }
        this.filters = filters;
        this.orders = orders;
    }

    /**
     * Construct a FlexQuery object based on Filters.
     *
     * @param filters Filter conditions
     */
    public FlexQuery(Filters filters) {
        this.filters = filters;
    }

    /**
     * Construct a FlexQuery object based on Filters and Orders.
     *
     * @param filters Filter conditions
     * @param orders  Sort rules
     */
    public FlexQuery(Filters filters, Orders orders) {
        this.filters = filters;
        this.orders = orders;
    }

    /**
     * Search in all timeline data, used to query and modify historical timeline rows
     */
    public FlexQuery acrossTimelineData() {
        this.acrossTimeline = true;
        return this;
    }

    /**
     * Whether to get all timeline slice data.
     * If the FlexQuery object contains effectiveStart or effectiveEnd,
     * it is also means across the timeline.
     */
    public boolean isAcrossTimeline() {
        if (acrossTimeline) {
            return true;
        } else if (!Filters.isEmpty(filters)) {
            Set<String> filterFields = filters.allFields();
            return filterFields.contains(ModelConstant.EFFECTIVE_START) || filterFields.contains(ModelConstant.EFFECTIVE_END);
        }
        return false;
    }

    /**
     * Update the subQuery conditions based on the ManyToOne/OneToOne fields in the cascade fields,
     * that is, the fields of the associated model to be read in the cascade.
     *
     * @param relationField Relation field
     * @param fields Field set of the associated model to be read in the cascade
     */
    public void expandSubQuery(String relationField, Collection<String> fields) {
        this.subQueries.put(relationField, new SubQuery(fields));
    }

    /**
     * Set SubQuery conditions
     * @param subQueries SubQuery condition Map
     */
    public void expandSubQueries(Map<String, SubQuery> subQueries) {
        if (!CollectionUtils.isEmpty(subQueries)) {
            this.subQueries.putAll(subQueries);
        }
    }

    /**
     * Get the SubQuery object based on the relational field
     * @param relationField Relation field
     * @return SubQuery object
     */
    public SubQuery extractSubQuery(String relationField) {
        SubQuery subQuery = this.subQueries.getOrDefault(relationField, null);
        return BeanTool.isAllFieldsNull(subQuery) ? null : subQuery;
    }

    /**
     * Add a field based on the lambda method reference
     * @param method Field lambda method reference
     * @return FlexQuery object
     */
    public <T, R> FlexQuery addField(SFunction<T, R> method) {
        String field = LambdaUtils.getAttributeName(method);
        if (this.fields != null) {
            this.fields.add(field);
        } else {
            this.fields = new HashSet<>(Collections.singletonList(field));
        }
        return this;
    }

    /**
     * Set the field list based on the lambda method reference
     * @param lambdaMethods Field lambda method reference
     */
    @SafeVarargs
    public final <T, R> FlexQuery setFields(SFunction<T, R>... lambdaMethods) {
        List<String> fields = Arrays.stream(lambdaMethods).map(LambdaUtils::getAttributeName).toList();
        if (!CollectionUtils.isEmpty(fields)) {
            this.fields = new HashSet<>(fields);
        }
        return this;
    }

    /**
     * Set the field list
     * @param fields Field list
     */
    public FlexQuery setFields(Collection<String> fields) {
        if (!CollectionUtils.isEmpty(fields)) {
            this.fields = new HashSet<>(fields);
        }
        return this;
    }

    public FlexQuery setGroupBy(String groupBy) {
        if (StringUtils.hasLength(groupBy)) {
            this.groupBy.add(groupBy);
            this.aggregate = true;
        }
        return this;
    }

    public FlexQuery setGroupBy(List<String> groupBy) {
        if (!CollectionUtils.isEmpty(groupBy)) {
            this.groupBy = groupBy;
            this.aggregate = true;
        }
        return this;
    }

    public FlexQuery setSplitBy(List<String> splitBy) {
        if (!CollectionUtils.isEmpty(splitBy)) {
            this.splitBy = splitBy;
            this.aggregate = true;
        }
        return this;
    }

    public FlexQuery setAggFunctions(AggFunctions aggFunctions) {
        if (!AggFunctions.isEmpty(aggFunctions)) {
            this.aggFunctions = aggFunctions;
            this.aggregate = true;
        }
        return this;
    }
}
