package info.openmeta.framework.orm.domain;

import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.base.utils.SFunction;
import info.openmeta.framework.orm.utils.LambdaUtils;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;

import java.text.MessageFormat;
import java.util.Collection;
import java.util.Objects;

/**
 * FilterUnit is the smallest component of a filter, represented as a three-element list: [field, operator, value].
 * Example: ["name", "=", "Tony"]. The value can be a single value or a collection value, and the collection value
 * is used for operators such as IN, NOT_IN, BETWEEN, NOT_BETWEEN, PARENT_OF, CHILD_OF. The value can be null when
 * the operator is IS_NULL or IS_NOT_NULL.
 */
@Data
@NoArgsConstructor
public class FilterUnit {

    public static final int UNIT_LENGTH = 3;

    private String field;
    private Operator operator;
    private Object value;


    public FilterUnit(String field, Operator operator, Object value) {
        this.field = field;
        this.operator = operator;
        this.value = value;
        validateFilterUnit(this);
    }

    /**
     * Create a leaf node with filterUnit parameters
     * @param field field name
     * @param operator operator object
     * @param value value
     * @return FilterUnit
     */
    public static FilterUnit of(String field, Operator operator, Object value) {
        return new FilterUnit(field, operator, value);
    }

    /**
     * Create a leaf node with filterUnit parameters, using Lambda expression as field method to extract field name.
     * @param method method reference, Lambda expression
     * @param operator operator object
     * @param value value
     * @return FilterUnit
     */
    public static <T, R> FilterUnit of(SFunction<T, R> method, Operator operator, Object value) {
        String field = LambdaUtils.getAttributeName(method);
        return of(field, operator, value);
    }

    /**
     * Validate the FilterUnit format
     * @param filterUnit FilterUnit
     */
    public static void validateFilterUnit(FilterUnit filterUnit) {
        Operator operator = filterUnit.getOperator();
        if (StringUtils.isEmpty(filterUnit.getField()) || operator == null) {
            throw new IllegalArgumentException("FilterUnit {0} field name and operator cannot be empty.", filterUnit);
        } else if (filterUnit.getValue() == null && !Operator.ASSIGNED_OPERATORS.contains(operator)) {
            // Inverse the EQUAL/NOT_EQUAL operators with null value, to IS_NOT_SET/IS_SET operators.
            if (Operator.EQUAL.equals(operator)) {
                filterUnit.setOperator(Operator.IS_NOT_SET);
            } else if (Operator.NOT_EQUAL.equals(operator)) {
                filterUnit.setOperator(Operator.IS_SET);
            } else {
                throw new IllegalArgumentException("FilterUnit {0} value cannot be empty.", filterUnit);
            }
        } else if (Operator.COMPARISON_OPERATORS.contains(operator)) {
            validateComparisonOperator(filterUnit);
        } else if (Operator.MATCHING_OPERATORS.contains(operator)) {
            validateMatchingOperators(filterUnit);
        } else if (Operator.COLLECTION_OPERATORS.contains(operator)) {
            validateCollectionValue(filterUnit);
        }
    }

    /**
     * Validate the value of the comparison operator, which can only be a single value, not a collection
     * @param filterUnit FilterUnit
     */
    private static void validateComparisonOperator(FilterUnit filterUnit) {
        Assert.notTrue(filterUnit.getValue() instanceof Collection,
                "The value of comparison operator can only be a single value: {0}", filterUnit);
    }

    /**
     * Validate the value of the matching operator, which can only be a string type
     * @param filterUnit FilterUnit
     */
    private static void validateMatchingOperators(FilterUnit filterUnit) {
        Object value = filterUnit.getValue();
        Assert.isTrue(value instanceof String && StringUtils.isNotBlank((String) value),
                "The value of matching operator can only be of string type: {0}", filterUnit);
    }

    /**
     * Validate the comparison value of the collection type.
     * Operators: IN, NOT_IN, BETWEEN, NOT_BETWEEN, PARENT_OF, CHILD_OF
     * @param filterUnit FilterUnit
     */
    private static void validateCollectionValue(FilterUnit filterUnit) {
        Operator operator = filterUnit.getOperator();
        if (filterUnit.getValue() instanceof Collection<?> valueList) {
            Assert.allNotNull(valueList, "The value cannot be null or include null item: {0}", filterUnit);
            if ((Operator.PARENT_OF.equals(operator) || Operator.CHILD_OF.equals(operator))) {
                valueList.forEach(v -> Assert.isTrue(v instanceof String && StringUtils.isNotBlank((String) v),
                        "The value of {0} operator can only be a list of non-empty strings: {1}", operator, filterUnit));
            } else if (Operator.BETWEEN.equals(operator) || Operator.NOT_BETWEEN.equals(operator)) {
                Assert.isTrue(valueList.size() == 2,
                        "The value of the {0} operator must be a list of two values: {1}", operator, filterUnit);
            }
        } else {
            throw new IllegalArgumentException("The value of the {0} operator can only be a list: {1}", operator, filterUnit);
        }
    }

    @Override
    public String toString() {
        String str = this.value instanceof String ? ("\"" + this.value + "\"") : JsonMapper.objectToString(this.value);
        return MessageFormat.format("[\"{0}\",\"{1}\",{2}]", this.field, this.operator.getName(), str);
    }

    public String toSemanticString() {
        String str = this.value instanceof String ? ("\"" + this.value + "\"") : JsonMapper.objectToString(this.value);
        return MessageFormat.format("{0} {1} {2}", this.field, this.operator.getName(), str);
    }

    @Override
    public boolean equals(Object filterUnit) {
        if (filterUnit instanceof FilterUnit) {
            return Objects.equals(field, ((FilterUnit) filterUnit).getField())
                    && Objects.equals(operator, ((FilterUnit) filterUnit).getOperator())
                    && Objects.equals(value, ((FilterUnit) filterUnit).getValue());
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(field, operator, value);
    }

    public FilterUnit copy() {
        return FilterUnit.of(field, operator, value);
    }
}
