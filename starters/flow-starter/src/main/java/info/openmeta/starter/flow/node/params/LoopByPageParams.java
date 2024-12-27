package info.openmeta.starter.flow.node.params;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.Orders;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

/**
 * Pagination loop processing parameters.
 * Business logic is processed within the loop based on paginated data.
 */
@Data
@Schema(name = "LoopByPageParams")
public class LoopByPageParams implements NodeParams {

    @Schema(description = "Model name for paginated data retrieval")
    private String model;

    @Schema(description = "Fields for paginated data retrieval; if empty, only the id field is read.")
    private List<String> fields;

    @Schema(description = """
            Filters for data retrieval, value supports constants, variables `#{}`, expressions `${}`,
            and reserved field name `@{}`.
            """)
    private Filters filters;

    @Schema(description = "Sorting conditions for paginated data retrieval.")
    private Orders orders;

    @Schema(description = "Page size; if not specified, the default size is used")
    private Integer pageSize;

    @Schema(description = "Loop parameter naming")
    private String pageParamNaming;
}
