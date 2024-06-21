package info.openmeta.framework.orm.domain;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.enums.AggFunctionType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * Aggregation function field
 */
@Getter
@Schema(name = "AggFunctionField")
public class AggFunctionField {

    @Schema(description = "Aggregation function type")
    private final AggFunctionType type;
    @Schema(description = "Aggregation field name")
    private final String field;
    @Schema(description = "Aggregation field alias")
    private final String alias;

    /**
     * The alias is the camel case string obtained by jointing the function name and the field name.
     * For example:
     *      sum + amount -> sumAmount,
     *      max + createdTime -> maxCreatedTime
     */
    public AggFunctionField(AggFunctionType type, String field) {
        Assert.notBlank(field, "Field cannot be empty");
        this.type = type;
        this.field = field;
        this.alias = type.getFunc() + Character.toUpperCase(field.charAt(0)) + field.substring(1);
    }

    @Override
    public String toString() {
        return this.type.name() + "(" + this.field + ") AS " + this.alias;
    }
}
