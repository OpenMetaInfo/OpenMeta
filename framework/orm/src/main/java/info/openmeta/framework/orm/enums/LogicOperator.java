package info.openmeta.framework.orm.enums;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Logic operator: AND, OR
 * Used for the logical relationship between multiple Filters, the default logical operator is AND
 */
@Getter
@AllArgsConstructor
public enum LogicOperator {
    AND,
    OR;

    // Default logical operator is AND
    public static final LogicOperator DEFAULT_LOGIC_OPERATOR = AND;

    /**
     * Convert the string to the Enum item
     * @param logicOperator logic operator string
     * @return LogicOperator object
     */
    public static LogicOperator of(String logicOperator) {
        Assert.notBlank(logicOperator, "Logic operator cannot be empty.");
        logicOperator = logicOperator.trim().toUpperCase();
        if (AND.name().equals(logicOperator)) {
            return AND;
        } else if (OR.name().equals(logicOperator)) {
            return OR;
        } else {
            throw new IllegalArgumentException("Logical operator not supported: {0}", logicOperator);
        }
    }
}
