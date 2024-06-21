package info.openmeta.framework.orm.domain;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Collection;

/**
 * Sub query conditions, used to specify the fields, filters, orders, pageNumber, pageSize of the sub query.
 * Application scenarios of different field types:
 *      1. ManyToOne, OneToOne: specify the field list to get.
 *      2. OneToMany, ManyToMany: specify the fields to get, filters, orders, pageNumber, pageSize.
 */
@Schema(name = "SubQuery")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class SubQuery {
    @Schema(description = "Sub query fields[].")
    private Collection<String> fields;

    @Schema(description = "Sub query filters.")
    private Filters filters;

    @Schema(description = "Sub query orders.")
    private Orders orders;

    @Schema(description = "Sub query page number, default 1.")
    private Integer pageNumber;

    @Schema(description = "Sub query page size, default 50.")
    private Integer pageSize;

    public SubQuery(Collection<String> fields) {
        this.fields = fields;
    }

}
