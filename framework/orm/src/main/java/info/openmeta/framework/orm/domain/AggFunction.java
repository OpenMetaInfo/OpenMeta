package info.openmeta.framework.orm.domain;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.enums.AggFunctionType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

/**
 * Represents an aggregation function configuration for use in queries.
 * This class encapsulates the details necessary to define how an aggregation
 * should be performed on a specific field.
 */
@Getter
@Schema(name = "AggFunction")
public class AggFunction {

    @Schema(description = "Aggregation function type")
    private final AggFunctionType type;
    @Schema(description = "Aggregation field name")
    private final String field;
    @Schema(description = "Aggregation field alias")
    private final String alias;

    /**
     * The alias is the camel case string obtained by jointing the function name and the field name.
     * For example,
     *      sum + amount → sumAmount,
     *      max + createdTime → maxCreatedTime
     */
    public AggFunction(AggFunctionType type, String field) {
        Assert.notBlank(field, "Field cannot be empty");
        this.type = type;
        this.field = field;
        this.alias = type.getFunc() + Character.toUpperCase(field.charAt(0)) + field.substring(1);
    }

    /**
     * The alias is a specified camel case string, such as `newestTime`.
     * For example, ["MAX", "createdTime", "newestTime"].
     *
     * @param type aggregation function type
     * @param field field name
     * @param alias custom field alias
     */
    public AggFunction(AggFunctionType type, String field, String alias) {
        Assert.notBlank(field, "Field cannot be empty");
        Assert.isTrue(StringTools.isFieldName(alias),
                "Field alias `{0}` must be a valid field name format.", alias);
        this.type = type;
        this.field = field;
        this.alias = alias;
    }

    @Override
    public String toString() {
        return this.type.name() + "(" + this.field + ") AS " + this.alias;
    }
}
