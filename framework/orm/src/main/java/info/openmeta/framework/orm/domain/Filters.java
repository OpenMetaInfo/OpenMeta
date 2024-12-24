package info.openmeta.framework.orm.domain;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import info.openmeta.framework.base.constant.StringConstant;
import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.base.utils.SFunction;
import info.openmeta.framework.orm.domain.antlr.FilterExprVisitorImpl;
import info.openmeta.framework.orm.domain.antlr.gen.FilterExprLexer;
import info.openmeta.framework.orm.domain.antlr.gen.FilterExprParser;
import info.openmeta.framework.orm.domain.serializer.FiltersDeserializer;
import info.openmeta.framework.orm.domain.serializer.FiltersSerializer;
import info.openmeta.framework.orm.enums.FilterType;
import info.openmeta.framework.orm.enums.LogicOperator;
import info.openmeta.framework.orm.utils.BeanTool;
import info.openmeta.framework.orm.utils.LambdaUtils;
import info.openmeta.framework.orm.utils.ListUtils;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.stream.Collectors;

import static info.openmeta.framework.orm.enums.FilterType.EMPTY;

/**
 * `Filters` object combines multiple query conditions and consists of FilterUnit objects.
 * A FilterUnit is the smallest component of a filter, represented as a three-element list: [field, operator, value].
 * Support nested filters, such as [a OR b] AND [c OR d OR [e AND f] OR g]; each element represents a FilterUnit object.
 * The nesting level is not limited for now, and each element is a FilterUnit.
 * So, there are 3 types of filters object: EMPTY, TREE, LEAF. Default type: EMPTY, equals to [].
 * The logic operator is AND by default, and the logic operator AND, OR is case-insensitive.
 * FilterUnit value cannot be null, when the Operator is `IS SET` or `IS NOT SET`, the value will be ignored.
 *  Examples of API usage:
 *      []
 *      ["title", "=", "PM"]
 *      [["title", "=", "PM"], ["grade", "=", 6]]
 *      [["title", "=", "PM"], "OR", ["code", "=", "A010"], "OR", ["grade", "=", 6]]
 *      [[["title", "=", "PM"], "OR", ["code", "=", "A010"]], "AND", ["grade", "=", 6]]
 *  Support value using @{fieldName} to reserve field name for field comparison.
 *  For example,
 *      ["updatedTime", ">", "@{createdTime}"] converts to sql: `updated_time > created_time`
 *  The reserved field name must be a field of the same model as the leftmost field name.
 * <p>
 *  Example of semantic query:
 *    name = "PM" OR (code = "A010" AND grade = 1)
 */
@Data
@NoArgsConstructor
@JsonSerialize(using = FiltersSerializer.class)
@JsonDeserialize(using = FiltersDeserializer.class)
@Schema(type = "array",
        example = "[\"name\", \"=\", \"PM\"]",
        description = """
                Support nested filters, such as [a OR b] AND [c OR d OR [e AND f] OR g]
                * []
                * ["title", "=", "PM"]
                * [["title", "=", "PM"], ["grade", "=", 6]]
                * [["title", "=", "PM"], "OR", ["code", "=", "A010"], "OR", ["grade", "=", 6]]
                * [["title", "=", "PM"], "OR", ["code", "=", "A010"]], "AND", ["grade", "=", 6]]
                """
)
public class Filters {

    @Schema(hidden = true)
    private FilterType type = EMPTY;

    @Schema(hidden = true)
    private LogicOperator logicOperator;

    @Schema(hidden = true)
    private List<Filters> children = new ArrayList<>();

    @Schema(hidden = true)
    private FilterUnit filterUnit;

    /**
     * Convert String type filters to filters object.
     * Support structured query: `[["title", "=", "PM"], "OR", ["code", "=", "A010"]]`
     * Semantic query: `name = "PM" OR (code = "A010" AND grade = 1)`
     * @param filterString String type filters
     * @return filters object
     */
    public static Filters of(String filterString) {
        if (StringUtils.isBlank(filterString)) {
            return null;
        } else if (filterString.trim().startsWith(StringConstant.LEFT_SQUARE_BRACKET)) {
            List<Object> filterList = JsonMapper.stringToObject(filterString, new TypeReference<>() {});
            return listToFilters(filterList);
        } else {
            return ofSemantic(filterString);
        }
    }

    /**
     * Convert semantic query string to filters object.
     * @param semanticString String type filters, e.g.: `name = "PM" OR (code = "A010" AND grade = 1)`
     * @return filters object
     */
    public static Filters ofSemantic(String semanticString) {
        if (StringUtils.isBlank(semanticString)) {
            return null;
        }
        CharStream input = CharStreams.fromString(semanticString);
        // Use the generated Lexer, Parser, Visitor, and Listener to parse the input text and generate the parse tree
        FilterExprLexer lexer = new FilterExprLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        FilterExprParser parser = new FilterExprParser(tokens);
        ParseTree tree = parser.expr();

        FilterExprVisitorImpl visitor = new FilterExprVisitorImpl();
        return visitor.visit(tree);
    }

    /**
     * Combine all properties of an entity object into a Filters object using the `EQUAL` operator and `AND` logic.
     *
     * @param entity entity object
     * @return filters object
     */
    public static <T> Filters ofEntity(T entity, boolean ignoreNull) {
        Map<String, Object> entityMap = BeanTool.objectToMap(entity, ignoreNull);
        Filters filters = new Filters();
        entityMap.forEach((key, value) -> {
            if (value != null) {
                filters.and(key, Operator.EQUAL, value);
            } else {
                filters.and(key, Operator.IS_NOT_SET, null);
            }
        });
        return filters;
    }

    /**
     * Convert List object to a filters object.
     * @param listObject List object, e.g.: [["title", "=", "PM"], "OR", ["grade", "=", 1]]
     * @return filters object
     */
    public static Filters of(List<?> listObject) {
        if (CollectionUtils.isEmpty(listObject)) {
            return null;
        }
        return listToFilters(listObject);
    }

    /**
     * Initialize a leaf node with field, operator, and value.
     *
     * @param field field name
     * @param operator operator
     * @param value value
     * @return filters instance
     */
    public static Filters of(String field, Operator operator, Object value) {
        return Filters.of(new FilterUnit(field, operator, value));
    }

    /**
     * Create a leaf node with filterUnit object.
     *
     * @param filterUnit filterUnit object
     * @return filters instance
     */
    public static Filters of(FilterUnit filterUnit) {
        Filters filters = new Filters();
        filters.setType(FilterType.LEAF);
        filters.setFilterUnit(filterUnit);
        return filters;
    }

    /**
     * Add a filterUnit to the current Filters object.
     *
     * @param method method reference of the field
     * @param operator operator
     * @param value value
     * @return the updated Filters object with the added filterUnit
     */
    public <T, R> Filters add(SFunction<T, R> method, Operator operator, Object value) {
        String field = LambdaUtils.getAttributeName(method);
        return this.add(field, operator, value);
    }

    /**
     * Add a filterUnit to the current Filters object.
     *
     * @param field fieldName
     * @param operator operator
     * @param value value
     * @return the updated Filters object with the added filterUnit
     */
    public Filters add(String field, Operator operator, Object value) {
        if (EMPTY.equals(this.getType())) {
            this.setType(FilterType.LEAF);
            this.setFilterUnit(FilterUnit.of(field, operator, value));
        } else if (FilterType.LEAF.equals(this.type)) {
            this.convertLeafToTree();
            this.children.add(Filters.of(field, operator, value));
        } else if (FilterType.TREE.equals(this.type)) {
            this.children.add(Filters.of(field, operator, value));
        } else {
            throw new IllegalArgumentException("Unsupported filter type: " + this.type);
        }
        return this;
    }

    /**
     * Add an EQUAL filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param value the comparison value
     * @return the updated filters
     */
    public <T, R> Filters eq(SFunction<T, R> method, Object value) {
        return this.add(method, Operator.EQUAL, value);
    }

    /**
     * Add an EQUAL filter to the current Filters object.
     * @param field field name
     * @param value the comparison value
     * @return the updated filters
     */
    public Filters eq(String field, Object value) {
        return this.add(field, Operator.EQUAL, value);
    }

    /**
     * Add a NOT_EQUAL filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param value the comparison value
     * @return the updated filters
     */
    public <T, R> Filters ne(SFunction<T, R> method, Object value) {
        return this.add(method, Operator.NOT_EQUAL, value);
    }

    /**
     * Add a NOT_EQUAL filter to the current Filters object.
     * @param field field name
     * @param value the comparison value
     * @return the updated filters
     */
    public Filters ne(String field, Object value) {
        return this.add(field, Operator.NOT_EQUAL, value);
    }

    /**
     * Add a GREATER_THAN filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param value the comparison value
     * @return the updated filters
     */
    public <T, R> Filters gt(SFunction<T, R> method, Object value) {
        return this.add(method, Operator.GREATER_THAN, value);
    }

    /**
     * Add a GREATER_THAN filter to the current Filters object.
     * @param field field name
     * @param value the comparison value
     * @return the updated filters
     */
    public Filters gt(String field, Object value) {
        return this.add(field, Operator.GREATER_THAN, value);
    }

    /**
     * Add a GREATER_THAN_OR_EQUAL filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param value the comparison value
     * @return the updated filters
     */
    public <T, R> Filters ge(SFunction<T, R> method, Object value) {
        return this.add(method, Operator.GREATER_THAN_OR_EQUAL, value);
    }

    /**
     * Add a GREATER_THAN_OR_EQUAL filter to the current Filters object.
     * @param field field name
     * @param value the comparison value
     * @return the updated filters
     */
    public Filters ge(String field, Object value) {
        return this.add(field, Operator.GREATER_THAN_OR_EQUAL, value);
    }

    /**
     * Add a LESS_THAN filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param value the comparison value
     * @return the updated filters
     */
    public <T, R> Filters lt(SFunction<T, R> method, Object value) {
        return this.add(method, Operator.LESS_THAN, value);
    }

    /**
     * Add a LESS_THAN filter to the current Filters object.
     * @param field field name
     * @param value the comparison value
     * @return the updated filters
     */
    public Filters lt(String field, Object value) {
        return this.add(field, Operator.LESS_THAN, value);
    }

    /**
     * Add a LESS_THAN_OR_EQUAL filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param value the comparison value
     * @return the updated filters
     */
    public <T, R> Filters le(SFunction<T, R> method, Object value) {
        return this.add(method, Operator.LESS_THAN_OR_EQUAL, value);
    }

    /**
     * Add a LESS_THAN_OR_EQUAL filter to the current Filters object.
     * @param field field name
     * @param value the comparison value
     * @return the updated filters
     */
    public Filters le(String field, Object value) {
        return this.add(field, Operator.LESS_THAN_OR_EQUAL, value);
    }

    /**
     * Add a CONTAINS filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param value the comparison value
     * @return the updated filters
     */
    public <T, R> Filters contains(SFunction<T, R> method, String value) {
        return this.add(method, Operator.CONTAINS, value);
    }

    /**
     * Add a CONTAINS filter to the current Filters object.
     * @param field field name
     * @param value the comparison value
     * @return the updated filters
     */
    public Filters contains(String field, String value) {
        return this.add(field, Operator.CONTAINS, value);
    }

    /**
     * Add a NOT_CONTAINS filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param value the comparison value
     * @return the updated filters
     */
    public <T, R> Filters notContains(SFunction<T, R> method, String value) {
        return this.add(method, Operator.NOT_CONTAINS, value);
    }

    /**
     * Add a NOT_CONTAINS filter to the current Filters object.
     * @param field field name
     * @param value the comparison value
     * @return the updated filters
     */
    public Filters notContains(String field, String value) {
        return this.add(field, Operator.NOT_CONTAINS, value);
    }

    /**
     * Add a START_WITH filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param value the comparison value
     * @return the updated filters
     */
    public <T, R> Filters startWith(SFunction<T, R> method, String value) {
        return this.add(method, Operator.START_WITH, value);
    }

    /**
     * Add a START_WITH filter to the current Filters object.
     * @param field field name
     * @param value the comparison value
     * @return the updated filters
     */
    public Filters startWith(String field, String value) {
        return this.add(field, Operator.START_WITH, value);
    }

    /**
     * Add a NOT_START_WITH filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param value the comparison value
     * @return the updated filters
     */
    public <T, R> Filters notStartWith(SFunction<T, R> method, String value) {
        return this.add(method, Operator.NOT_START_WITH, value);
    }

    /**
     * Add a NOT_START_WITH filter to the current Filters object.
     * @param field field name
     * @param value the comparison value
     * @return the updated filters
     */
    public Filters notStartWith(String field, String value) {
        return this.add(field, Operator.NOT_START_WITH, value);
    }

    /**
     * Add an IN filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param value the comparison value
     * @return the updated filters
     */
    public <T, R> Filters in(SFunction<T, R> method, Collection<?> value) {
        return this.add(method, Operator.IN, value);
    }

    /**
     * Add an IN filter to the current Filters object.
     * @param field field name
     * @param value the comparison value
     * @return the updated filters
     */
    public Filters in(String field, Collection<?> value) {
        return this.add(field, Operator.IN, value);
    }

    /**
     * Add a NOT_IN filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param value the comparison value
     * @return the updated filters
     */
    public <T, R> Filters notIn(SFunction<T, R> method, Collection<?> value) {
        return this.add(method, Operator.NOT_IN, value);
    }

    /**
     * Add a NOT_IN filter to the current Filters object.
     * @param field field name
     * @param value the comparison value
     * @return the updated filters
     */
    public Filters notIn(String field, Collection<?> value) {
        return this.add(field, Operator.NOT_IN, value);
    }

    /**
     * Add a BETWEEN filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param value1 the first comparison value
     * @param value2 the second comparison value
     * @return the updated filters
     */
    public <T, R> Filters between(SFunction<T, R> method, Object value1, Object value2) {
        Assert.isTrue(value1 != null && value2 != null, "The comparison value cannot be null.");
        return this.add(method, Operator.BETWEEN, Arrays.asList(value1, value2));
    }

    /**
     * Add a BETWEEN filter to the current Filters object.
     * @param field field name
     * @param value1 the first comparison value
     * @param value2 the second comparison value
     * @return the updated filters
     */
    public Filters between(String field, Object value1, Object value2) {
        Assert.isTrue(value1 != null && value2 != null, "The comparison value cannot be null.");
        return this.add(field, Operator.BETWEEN, Arrays.asList(value1, value2));
    }

    /**
     * Add a NOT_BETWEEN filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param value1 the first comparison value
     * @param value2 the second comparison value
     * @return the updated filters
     */
    public <T, R> Filters notBetween(SFunction<T, R> method, Object value1, Object value2) {
        Assert.isTrue(value1 != null && value2 != null, "The comparison value cannot be null.");
        return this.add(method, Operator.NOT_BETWEEN, Arrays.asList(value1, value2));
    }

    /**
     * Add a NOT_BETWEEN filter to the current Filters object.
     * @param field field name
     * @param value1 the first comparison value
     * @param value2 the second comparison value
     * @return the updated filters
     */
    public Filters notBetween(String field, Object value1, Object value2) {
        Assert.isTrue(value1 != null && value2 != null, "The comparison value cannot be null.");
        return this.add(field, Operator.NOT_BETWEEN, Arrays.asList(value1, value2));
    }

    /**
     * Add an IS_SET filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @return the updated filters
     */
    public <T, R> Filters isSet(SFunction<T, R> method) {
        return this.add(method, Operator.IS_SET, null);
    }

    /**
     * Add an IS_SET filter to the current Filters object.
     * @param field field name
     * @return the updated filters
     */
    public Filters isSet(String field) {
        return this.add(field, Operator.IS_SET, null);
    }

    /**
     * Add an IS_NOT_SET filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @return the updated filters
     */
    public <T, R> Filters isNotSet(SFunction<T, R> method) {
        return this.add(method, Operator.IS_NOT_SET, null);
    }

    /**
     * Add an IS_NOT_SET filter to the current Filters object.
     * @param field field name
     * @return the updated filters
     */
    public Filters isNotSet(String field) {
        return this.add(field, Operator.IS_NOT_SET, null);
    }

    /**
     * Add a PARENT_OF filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param idPath the string value of the id path
     * @return the updated filters
     */
    public <T, R> Filters parentOf(SFunction<T, R> method, String idPath) {
        Assert.notBlank(idPath, "The idPath cannot be blank.");
        return this.add(method, Operator.PARENT_OF, Collections.singletonList(idPath));
    }

    /**
     * Add a PARENT_OF filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param idPaths the collection of id paths
     * @return the updated filters
     */
    public <T, R> Filters parentOf(SFunction<T, R> method, Collection<?> idPaths) {
        return this.add(method, Operator.PARENT_OF, idPaths);
    }

    /**
     * Add a PARENT_OF filter to the current Filters object.
     * @param field field name
     * @param idPath the string value of the id path
     * @return the updated filters
     */
    public Filters parentOf(String field, String idPath) {
        Assert.notBlank(idPath, "The idPath cannot be blank.");
        return this.add(field, Operator.PARENT_OF, Collections.singletonList(idPath));
    }

    /**
     * Add a PARENT_OF filter to the current Filters object.
     * @param field field name
     * @param idPaths the collection of id paths
     * @return the updated filters
     */
    public Filters parentOf(String field, Collection<?> idPaths) {
        return this.add(field, Operator.PARENT_OF, idPaths);
    }

    /**
     * Add a CHILD_OF filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param idPath the string value of the id path
     * @return the updated filters
     */
    public <T, R> Filters childOf(SFunction<T, R> method, String idPath) {
        Assert.notBlank(idPath, "The idPath cannot be blank.");
        return this.add(method, Operator.CHILD_OF, Collections.singletonList(idPath));
    }

    /**
     * Add a CHILD_OF filter to the current Filters object using method reference (lambda expression).
     * @param method the method reference of the field
     * @param idPaths the collection of id paths
     * @return the updated filters
     */
    public <T, R> Filters childOf(SFunction<T, R> method, Collection<?> idPaths) {
        return this.add(method, Operator.CHILD_OF, idPaths);
    }

    /**
     * Add a CHILD_OF filter to the current Filters object.
     * @param field field name
     * @param idPath the string value of the id path
     * @return the updated filters
     */
    public Filters childOf(String field, String idPath) {
        Assert.notBlank(idPath, "The idPath cannot be blank.");
        return this.add(field, Operator.CHILD_OF, Collections.singletonList(idPath));
    }

    /**
     * Add a CHILD_OF filter to the current Filters object.
     * @param field field name
     * @param idPaths the collection of id paths
     * @return the updated filters
     */
    public Filters childOf(String field, Collection<?> idPaths) {
        return this.add(field, Operator.CHILD_OF, idPaths);
    }

    /**
     * Determine if the filters object is empty, including null, []
     *
     * @param filters object
     * @return is empty or not
     */
    public static boolean isEmpty(Filters filters) {
        if (filters == null || EMPTY.equals(filters.getType()) || (filters.getFilterUnit() == null && filters.getChildren().isEmpty())) {
            return true;
        } else if (filters.getChildren().size() == 1) {
            return Filters.isEmpty(filters.getChildren().getFirst());
        }
        return false;
    }

    /**
     * Static method to create a new `AND` logical filters.
     *
     * @return empty filters object with `AND` logic
     */
    public static Filters and() {
        Filters filters = new Filters();
        filters.setType(FilterType.TREE);
        filters.setLogicOperator(LogicOperator.AND);
        return filters;
    }

    /**
     * Add a Filters object to `AND` logical filters.
     *
     * @param filters filters object
     * @return filters after `AND`
     */
    public Filters and(Filters filters) {
        return this.combine(LogicOperator.AND, filters);
    }

    /**
     * Combine multiple filters into a new `AND` logical filters.
     *
     * @param filters1 filters object1
     * @param filters2 filters object2
     * @param filtersArray the variable number of filters objects
     * @return filters after combined with `AND` logic
     */
    public static Filters and(Filters filters1, Filters filters2, Filters... filtersArray) {
        return combine(LogicOperator.AND, filters1, filters2, filtersArray);
    }

    /**
     * Combine a list of filters into a new `AND` logical filters.
     *
     * @param filtersList list of filters
     * @return new filters object after combined with `AND` logic
     */
    public static Filters and(List<Filters> filtersList) {
        if (CollectionUtils.isEmpty(filtersList)) {
            return null;
        }
        Filters combinedFilters = and();
        combinedFilters.setChildren(filtersList.stream().filter(f -> !Filters.isEmpty(f)).toList());
        return combinedFilters;
    }

    /**
     * Combine current filters with a new filter using `AND` logic.
     *
     * @param field field
     * @param operator operator
     * @param value value
     */
    public Filters and(String field, Operator operator, Object value) {
        return this.and(Filters.of(field, operator, value));
    }

    /**
     * Static method to create a new `OR` logical filters.
     * Example: Filters.or().eq(Job::getTitle, "PM").eq(Job::getGrade, 6)
     *
     * @return filters after `OR`
     */
    public static Filters or() {
        Filters filters = new Filters();
        filters.setType(FilterType.TREE);
        filters.setLogicOperator(LogicOperator.OR);
        return filters;
    }

    /**
     * Add a Filters object to `OR` logical filters.
     *
     * @param filters filters object
     * @return filters after `OR`
     */
    public Filters or(Filters filters) {
        return this.combine(LogicOperator.OR, filters);
    }

    /**
     * Combine current filters with a new filter using `OR` logic.
     *
     * @param field field name
     * @param operator operator
     * @param value the comparison value
     * @return filters after `OR`
     */
    public Filters or(String field, Operator operator, Object value) {
        return this.or(Filters.of(field, operator, value));
    }

    /**
     * Combine multiple filters into a new `OR` logical filters.
     *
     * @param filters1 filters object1
     * @param filters2 filters object2
     * @param filtersArray the variable number of filters objects
     * @return filters after combined with `OR` logic
     */
    public static Filters or(Filters filters1, Filters filters2, Filters... filtersArray) {
        return combine(LogicOperator.OR, filters1, filters2, filtersArray);
    }

    /**
     * Convert LEAF type filters to TREE type filters.
     */
    private void convertLeafToTree() {
        this.setType(FilterType.TREE);
        this.children.add(of(this.filterUnit));
        this.filterUnit = null;
        if (this.logicOperator == null) {
            this.logicOperator = LogicOperator.DEFAULT_LOGIC_OPERATOR;
        }
    }

    /**
     * Combine current filters with a new filter using specified logic operator.
     * e.g. f1.combine(logicOperator, f2)
     *
     * @param logicOperator logic operator
     * @param filters filters object
     * @return updated filters object
     */
    private Filters combine(LogicOperator logicOperator, Filters filters) {
        if (Filters.isEmpty(filters)) {
            return this;
        } else if (EMPTY.equals(this.getType())) {
            this.setType(FilterType.TREE);
            this.setLogicOperator(logicOperator);
            this.children.add(filters);
        } else if (FilterType.TREE.equals(this.type)) {
            if (!Objects.equals(this.logicOperator, logicOperator)) {
                Filters originalFilters = this.shallowCopy();
                this.logicOperator = logicOperator;
                this.setChildren(ListUtils.of(originalFilters, filters));
            } else {
                this.children.add(filters);
            }
        } else if (FilterType.LEAF.equals(this.type)) {
            this.logicOperator = logicOperator;
            this.convertLeafToTree();
            this.children.add(filters);
        }
        return this;
    }

    /**
     * Combine multiple filters into a new filters object using specified logic operator.
     *
     * @param logicOperator logic operator, `OR` or `AND`.
     * @param filtersArray filters object array
     * @return new filters object after combined
     */
    private static Filters combine(LogicOperator logicOperator, Filters filters1, Filters filters2, Filters... filtersArray) {
        // Just combine filters which are not empty.
        List<Filters> filtersList = new ArrayList<>();
        if (!Filters.isEmpty(filters1)) {
            filtersList.add(filters1);
        }
        if (!Filters.isEmpty(filters2)) {
            filtersList.add(filters2);
        }
        filtersList.addAll(Arrays.stream(filtersArray).filter(f -> !Filters.isEmpty(f)).toList());
        // If all filters are empty, return null.
        if (filtersList.isEmpty()) {
            return null;
        } else if (filtersList.size() == 1) {
            return filtersList.getFirst();
        } else {
            // Default is `AND` logic.
            Filters combinedFilters = LogicOperator.OR.equals(logicOperator)? or() : and();
            combinedFilters.setChildren(filtersList);
            return combinedFilters;
        }
    }

    /**
     * Extract field names from Filters.
     *
     * @return field name set
     */
    public Set<String> extractFields() {
        Set<String> fields = new HashSet<>();
        if (FilterType.TREE.equals(this.type)) {
            this.children.forEach(filters -> fields.addAll(filters.extractFields()));
        } else if (FilterType.LEAF.equals(this.type)) {
            fields.add(this.filterUnit.getField());
        }
        return fields;
    }

    /**
     * Determine if the filters object contains the specified field.
     *
     * @param filters filters object
     * @param field field name
     * @return contains or not
     */
    public static boolean containsField(Filters filters, String field) {
        if (Filters.isEmpty(filters)) {
            return false;
        }
        return filters.extractFields().contains(field);
    }

    /**
     * Convert original list object to filters object. Trim field and operator.
     *
     * @param filterList the original list object to be converted.
     * @return filters object
     */
    private static Filters listToFilters(List<?> filterList) {
        if (filterList.isEmpty()) {
            // Empty filters: [], which will be ignored in the final SQL.
            return new Filters();
        } else if (filterList.get(0) instanceof String && filterList.size() == FilterUnit.UNIT_LENGTH) {
            // LEAF type filters: ["title", "=", "PM"]
            return Filters.of(((String) filterList.get(0)).trim(), Operator.of(((String) filterList.get(1)).trim()), filterList.get(2));
        } else if (filterList.get(0) instanceof List) {
            if (filterList.size() == 1) {
                // Remove redundant [] noise, such as: [[]], [["title", "=", "PM"]], [[[leaf1], "AND", [leaf2]]]
                return listToFilters((List<?>) filterList.getFirst());
            } else {
                return parseList(filterList);
            }
        } else {
            throw new IllegalArgumentException("Exception occurs in filters: {0}", filterList);
        }
    }

    /**
     * Parse FilterUnit and logical operator filters: [leaf1, "OR", leaf2], and trim the logical operator.
     * Such as: [["title", "=", "PM"], "OR", ["code", "=", "A010"]].
     *
     * @param filterList the original list object to be converted
     * @return parsed filters object
     */
    private static Filters parseList(List<?> filterList) {
        Filters filters = new Filters();
        filters.setType(FilterType.TREE);
        for (Object node : filterList) {
            if (node instanceof List) {
                Filters child = listToFilters((List<?>) node);
                // Ignore EMPTY node
                if (!Filters.isEmpty(child)) {
                    filters.getChildren().add(child);
                }
            } else if (node instanceof String) {
                parseLogicOperator(filters, ((String) node), filterList);
            } else {
                throw new IllegalArgumentException("Node type exception of filters: {0}", filterList);
            }
            if (filters.getChildren().size() > 1 && filters.getLogicOperator() == null) {
                // When there are multiple nodes and no logical operator is provided,
                // use the default logical operator AND.
                filters.setLogicOperator(LogicOperator.DEFAULT_LOGIC_OPERATOR);
            }
        }
        if (filters.getChildren().size() == 1) {
            // When there is only one valid children node, convert TREE type to LEAF type.
            // For example, convert: [["title", "=", "PM"], "OR"]
            // or: [["title", "=", "PM"], "OR", [], []]
            // to: ["title", "=", "PM"]
            filters.setType(FilterType.LEAF);
            filters.setLogicOperator(null);
            filters = filters.getChildren().getFirst();
        }
        return filters;
    }

    /**
     * Parse the string of logical operator.
     *
     * @param filters filters
     * @param operatorStr logical operator
     * @param filterList original list to be parsed
     */
    private static void parseLogicOperator(Filters filters, String operatorStr, List<?> filterList) {
        LogicOperator operator = LogicOperator.of(operatorStr);
        if (filters.getLogicOperator() == null) {
            filters.setLogicOperator(operator);
        } else if (!operator.equals(filters.getLogicOperator())) {
            throw new IllegalArgumentException("The logic operator is not unique: {0}", filterList);
        }
    }

    /**
     * Filters to JSON format String, EMPTY filters to "[]".
     *
     * @return string
     */
    @Override
    public String toString() {
        if (FilterType.TREE.equals(this.type)) {
            if (this.children.size() == 1) {
                return this.children.getFirst().toString();
            } else if (this.children.size() > 1) {
                return "[" + this.children.stream().map(Filters::toString).collect(Collectors.joining(",\"" + this.logicOperator + "\",")) + "]";
            }
        } else if (FilterType.LEAF.equals(this.type)) {
            return this.filterUnit.toString();
        }
        return "[]";
    }

    /**
     * Semantic String of Filters, for frontend interaction scenarios.
     *
     * @return string
     */
    public String toSemanticString() {
        return toSemanticStringRecursively(true);
    }

    /**
     * Semantic String of Filters, for frontend interaction scenarios.
     *
     * @param isRoot is root node
     * @return string
     */
    private String toSemanticStringRecursively(boolean isRoot) {
        if (FilterType.TREE.equals(this.type)) {
            if (this.children.size() == 1) {
                return this.children.getFirst().toSemanticStringRecursively(false);
            } else if (this.children.size() > 1) {
                String joinStr = this.children.stream().map(child -> child.toSemanticStringRecursively(false))
                        .collect(Collectors.joining(" " + this.logicOperator + " "));
                return isRoot ? joinStr : "(" + joinStr + ")";
            }
        } else if (FilterType.LEAF.equals(this.type)) {
            return this.filterUnit.toSemanticString();
        }
        return "";
    }

    @Override
    public boolean equals(Object filters) {
        if (filters instanceof Filters) {
            return Objects.equals(type, ((Filters) filters).getType())
                    && Objects.equals(logicOperator, ((Filters) filters).getLogicOperator())
                    && Objects.equals(children, ((Filters) filters).getChildren())
                    && Objects.equals(filterUnit, ((Filters) filters).getFilterUnit());
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(type, logicOperator, children, filterUnit);
    }

    /**
     * Filters object shallow copy, internal children objects remain unchanged.
     *
     * @return new copy object
     */
    private Filters shallowCopy() {
        Filters filters = new Filters();
        filters.setType(this.type);
        filters.setLogicOperator(this.logicOperator);
        filters.setChildren(new ArrayList<>(this.children));
        if (this.filterUnit != null) {
            filters.setFilterUnit(this.filterUnit);
        }
        return filters;
    }

    /**
     * Deep copy of Filters object.
     *
     * @return new copy object
     */
    public Filters deepCopy() {
        Filters filters = new Filters();
        filters.setType(this.type);
        filters.setLogicOperator(this.logicOperator);
        filters.setChildren(this.children.stream().map(Filters::deepCopy).collect(Collectors.toList()));
        if (this.filterUnit != null) {
            filters.setFilterUnit(this.filterUnit.copy());
        }
        return filters;
    }

}
