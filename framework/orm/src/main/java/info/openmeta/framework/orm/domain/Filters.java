package info.openmeta.framework.orm.domain;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import info.openmeta.framework.base.constant.StringConstant;
import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.base.exception.IllegalArgumentException;
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
 * Examples:
 *      []
 *      ["name", "=", "Tom"]
 *      [["name", "=", "Tom"], ["version", "=", "6"]]
 *      [["name", "=", "Tom"], "OR", ["code", "=", "A010"], "OR", ["version", "=", "2"]]
 *      [["name", "=", "Tom"], "OR", ["code", "=", "A010"]], "AND", ["version", "=", "2"]]
 *  <p>
 *  Support value using @{fieldName} to reserve field name for field comparison.
 *  For example,
 *      ["updatedTime", ">", "@{createdTime}"] converts to sql: `updated_time > created_time`
 *  The reserved field name must be a field of the same model as the leftmost field name.
 */
@Data
@NoArgsConstructor
@JsonSerialize(using = FiltersSerializer.class)
@JsonDeserialize(using = FiltersDeserializer.class)
@Schema(type = "array",
        example = "[\"name\", \"=\", \"Tom\"]",
        description = """
                Support nested filters, such as [a OR b] AND [c OR d OR [e AND f] OR g]
                * []
                * ["name", "=", "Tom"]
                * [["name", "=", "Tom"], ["version", "=", "6"]]
                * [["name", "=", "Tom"], "OR", ["code", "=", "A010"], "OR", ["version", "=", "2"]]
                * [["name", "=", "Tom"], "OR", ["code", "=", "A010"]], "AND", ["version", "=", "2"]]
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
     * Support structured query: "[["name", "=", "Test"], "OR", ["code", "=", "A01"]]"
     * Semantic query: "name = \"Test\" OR (code = \"A01\" AND version = 1)"
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
     * @param semanticString String type filters, e.g.: name = "Test OR (code = "A01" AND version = 1)
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
     * @param listObject List object, e.g.: [["name", "=", "Test"], "OR", ["amount", "=", 1]]
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
     * Initialize a leaf node with method reference (lambda expression), operator, and value.
     *
     * @param method field method, Lambda expression, method reference passing parameters
     * @param operator operator
     * @param value value
     * @return filters instance
     */
    public static <T, R> Filters of(SFunction<T, R> method, Operator operator, Object value) {
        String field = LambdaUtils.getAttributeName(method);
        return of(field, operator, value);
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
     * Create a leaf node with field, EQUAL operator, and value.
     *
     * @param field field name
     * @param value value
     * @return filters instance
     */
    public static Filters eq(String field, Object value) {
        return Filters.of(field, Operator.EQUAL, value);
    }

    /**
     * Create a leaf node with method reference (lambda expression), EQUAL operator, and value.
     *
     * @param method field method, Lambda expression, method reference passing parameters
     * @param value value
     * @return filters instance
     */
    public static <T, R> Filters eq(SFunction<T, R> method, Object value) {
        return Filters.of(method, Operator.EQUAL, value);
    }

    /**
     * Create a leaf node with field, IN operator, and value.
     *
     * @param field field name
     * @param value value
     * @return filters instance
     */
    public static Filters in(String field, Object value) {
        return Filters.of(field, Operator.IN, value);
    }

    /**
     * Create a leaf node with method reference (lambda expression), IN operator, and value.
     *
     * @param method field method, Lambda expression, method reference passing parameters
     * @param value value
     * @return filters instance
     */
    public static <T, R> Filters in(SFunction<T, R> method, Object value) {
        return Filters.of(method, Operator.IN, value);
    }

    public Filters between(String field, List<?> value) {
        return Filters.of(field, Operator.BETWEEN, value);
    }

    public <T, R> Filters between(SFunction<T, R> method, List<?> value) {
        String field = LambdaUtils.getAttributeName(method);
        return Filters.of(field, Operator.BETWEEN, value);
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
     * Add an FilterUnit to `AND` logical filters.
     *
     * @param field field
     * @param operator operator
     * @param value value
     */
    public Filters and(String field, Operator operator, Object value) {
        return this.and(Filters.of(field, operator, value));
    }

    /**
     * Add an FilterUnit to `AND` logical filters through method reference.
     *
     * @param method field method, Lambda expression, method reference passing parameters
     * @param operator operator
     * @param value value
     */
    public <T, R> Filters and(SFunction<T, R> method, Operator operator, Object value) {
        String field = LambdaUtils.getAttributeName(method);
        return this.and(Filters.of(field, operator, value));
    }

    /**
     * Add a FilterUnit to `AND` logical filters.
     *
     * @param filterUnit object
     */
    public void and(FilterUnit filterUnit) {
        Filters filters = of(filterUnit);
        this.and(filters);
    }

    /**
     * Add a child Filters object to `AND` logical filters.
     *
     * @param filters filters object
     * @return filters after `AND`
     */
    public Filters and(Filters filters) {
        if (Filters.isEmpty(filters)) {
            return this;
        }
        if (FilterType.TREE.equals(this.type) && LogicOperator.OR.equals(this.logicOperator)) {
            this.logicOperator = LogicOperator.AND;
            Filters originalFilters = this.shallowCopy();
            this.setChildren(Arrays.asList(originalFilters, filters));
        } else if (FilterType.LEAF.equals(this.type)) {
            this.transferType();
        }
        this.type = FilterType.TREE;
        this.logicOperator = LogicOperator.AND;
        this.children.add(filters);
        return this;
    }

    /**
     * And an `EQUAL` filterUnit to `AND` logical filters.
     *
     * @param field field
     * @param value value
     * @return filters after `AND`
     */
    public Filters andEq(String field, Object value) {
        return this.and(field, Operator.EQUAL, value);
    }

    /**
     * And an `EQUAL` filterUnit to `AND` logical filters.
     *
     * @param method field method, Lambda expression, method reference passing parameters
     * @param value value
     * @return filters after `AND`
     */
    public <T, R> Filters andEq(SFunction<T, R> method, Object value) {
        String field = LambdaUtils.getAttributeName(method);
        return this.and(Filters.eq(field, value));
    }

    /**
     * Create a leaf node with field, NOT_EQUAL operator, and value.
     *
     * @param field field name
     * @param value value
     * @return filters instance
     */
    public Filters andNe(String field, Object value) {
        return this.and(field, Operator.NOT_EQUAL, value);
    }

    /**
     * Create a leaf node with field, LESS_THAN_OR_EQUAL operator, and value.
     * @param field field name
     * @param value value
     * @return filters instance
     */
    public Filters andLe(String field, Object value) {
        return this.and(field, Operator.LESS_THAN_OR_EQUAL, value);
    }

    /**
     * Create a leaf node with field, GREATER_THAN_OR_EQUAL operator, and value.
     * @param field field name
     * @param value value
     * @return filters instance
     */
    public Filters andGe(String field, Object value) {
        return this.and(field, Operator.GREATER_THAN_OR_EQUAL, value);
    }

    /**
     * Create a leaf node with field, GREATER_THAN_OR_EQUAL operator, and value.
     * @param method field method, Lambda expression, method reference passing parameters
     * @param value value
     * @return filters instance
     */
    public <T, R> Filters andGe(SFunction<T, R> method, Object value) {
        String field = LambdaUtils.getAttributeName(method);
        return this.and(field, Operator.GREATER_THAN_OR_EQUAL, value);
    }

    /**
     * Create a leaf node with field, GREATER_THAN operator, and value.
     *
     * @param field field name
     * @param value value
     * @return filters instance
     */
    public Filters andGt(String field, Object value) {
        return this.and(field, Operator.GREATER_THAN, value);
    }

    public Filters andIn(String field, Object value) {
        return this.and(Filters.in(field, value));
    }

    public Filters andNotIn(String field, Collection<?> value) {
        return this.and(Filters.of(field, Operator.NOT_IN, value));
    }

    public <T, R> Filters andIn(SFunction<T, R> method, Collection<?> value) {
        String field = LambdaUtils.getAttributeName(method);
        return this.and(Filters.in(field, value));
    }

    /**
     * Merge multiple filters into a new `AND` logical filters.
     * The internal of a single filters may be `OR` or `AND`.
     *
     * @param filtersArray filters object array
     * @return filters after merge
     */
    public static Filters merge(Filters... filtersArray) {
        return merge(LogicOperator.AND, filtersArray);
    }

    /**
     * Merge multiple filters into a new filters object with specified logic operator.
     * The internal of a single filters may be `OR` or `AND`.
     *
     * @param logicOperator logic operator, `OR` or `AND`.
     * @param filtersArray filters object array
     * @return filters after merge
     */
    public static Filters merge(LogicOperator logicOperator, Filters... filtersArray) {
        // Just merge filters which is not empty.
        List<Filters> filtersList =  Arrays.stream(filtersArray).filter(f -> !Filters.isEmpty(f)).collect(Collectors.toList());
        if (filtersList.isEmpty()) {
            return null;
        } else if (filtersList.size() == 1) {
            return filtersList.getFirst();
        } else {
            Filters combinedFilters = new Filters();
            combinedFilters.setType(FilterType.TREE);
            combinedFilters.setLogicOperator(logicOperator);
            combinedFilters.setChildren(filtersList);
            return combinedFilters;
        }
    }

    /**
     * Add a filterUnit to `OR` logical filters.
     *
     * @param field field
     * @param operator operator
     * @param value value
     * @return filters after `OR`
     */
    public Filters or(String field, Operator operator, Object value) {
        return this.or(Filters.of(field, operator, value));
    }

    /**
     * Add a filterUnit with reversed field to `OR` logical filters.
     *
     * @param leftField left field
     * @param operator operator
     * @param rightField right field
     * @return filters after `OR`
     */
    public Filters orReserved(String leftField, Operator operator, String rightField) {
        String reversedField = StringConstant.RESERVED_PREFIX + rightField + "}";
        return this.or(Filters.of(leftField, operator, reversedField));
    }

    /**
     * Add a filterUnit to `OR` logical filters through method reference.
     *
     * @param method field method, Lambda expression, method reference passing parameters
     * @param operator operator
     * @param value value
     * @return filters after `OR`
     */
    public <T, R> Filters or(SFunction<T, R> method, Operator operator, Object value) {
        String field = LambdaUtils.getAttributeName(method);
        return or(field, operator, value);
    }

    /**
     * Add a filterUnit to `OR` logical filters.
     *
     * @param filterUnit object
     */
    public void or(FilterUnit filterUnit) {
        Filters filters = of(filterUnit);
        this.or(filters);
    }

    /**
     * Add a child Filters object to `OR` logical filters.
     *
     * @param filters filters object
     * @return filters after `OR`
     */
    public Filters or(Filters filters) {
        if (Filters.isEmpty(filters)) {
            return this;
        }
        if (FilterType.TREE.equals(this.type) && LogicOperator.AND.equals(this.logicOperator)) {
            this.logicOperator = LogicOperator.OR;
            Filters originalFilters = this.shallowCopy();
            this.setChildren(Arrays.asList(originalFilters, filters));
        } else if (FilterType.LEAF.equals(this.type)) {
            this.transferType();
        }
        this.type = FilterType.TREE;
        this.logicOperator = LogicOperator.OR;
        this.children.add(filters);
        return this;
    }

    /**
     * Convert LEAF type filters to TREE type filters.
     */
    private void transferType() {
        this.children.add(of(this.filterUnit.getField(), this.filterUnit.getOperator(), this.filterUnit.getValue()));
        this.filterUnit = null;
    }

    /**
     * Add a child filters to current filters.
     *
     * @param filters child filters
     */
    public void addChild(Filters filters){
        if (filters == null) {
            return;
        }
        this.children.add(filters);
    }

    /**
     * Extract field names from Filters.
     *
     * @return field name set
     */
    public Set<String> allFields() {
        Set<String> fields = new HashSet<>();
        if (FilterType.TREE.equals(this.type)) {
            this.children.forEach(filters -> fields.addAll(filters.allFields()));
        } else if (FilterType.LEAF.equals(this.type)) {
            fields.add(this.filterUnit.getField());
        }
        return fields;
    }

    /**
     * Determine if the filters object contains the specified field.
     *
     * @param field field name
     * @return contains or not
     */
    public boolean containsField(String field) {
        return allFields().contains(field);
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
        return filters.allFields().contains(field);
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
            // LEAF type filters: ["name", "=", "Test"]
            return Filters.of(((String) filterList.get(0)).trim(), Operator.of(((String) filterList.get(1)).trim()), filterList.get(2));
        } else if (filterList.get(0) instanceof List) {
            if (filterList.size() == 1) {
                // Remove redundant [] noise, such as: [[]], [["name", "=", "Tom"]], [[[leaf1], "AND", [leaf2]]]
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
     * Such as: [["name", "=", "Tom"], "OR", ["code", "=", "A010"]].
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
                if (child.getType() != EMPTY) {
                    filters.addChild(child);
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
            // For example, convert: [["name", "=", "Test"], "OR"]
            // or: [["name", "=", "Test"], "OR", [], []]
            // to: ["name", "=", "Test"]
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
