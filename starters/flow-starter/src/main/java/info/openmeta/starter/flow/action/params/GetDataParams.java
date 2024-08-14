package info.openmeta.starter.flow.action.params;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.starter.flow.action.enums.ActionGetDataType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * Parameters for getting data based on model name, fields, filters, orders, and other settings.
 * The total data obtained does not exceed `MAX_BATCH_SIZE`.
 */
@Schema(name = "Get Data Params")
@Data
@NoArgsConstructor
public class GetDataParams implements ActionParams {

    @Schema(description = "The model name of the data to be obtained")
    private String model;

    @Schema(description = """
            The data format type to be obtained, supports multi-row, single-row, single-field value,
            single-field value list, existence, and quantity statistics""")
    private ActionGetDataType getDataType;

    @Schema(description = "The field list of the data to be obtained, empty to obtain only the id field")
    private List<String> fields;

    @Schema(description = """
            The filters of the data to be obtained, value supports constants, variables `#{}`,
            calculation formulas `${}`, reserved field names `@{}`""")
    private Filters filters;

    @Schema(description = "The order of the data to be obtained.")
    private Orders orders;

    @Schema(description = "Whether to obtain data across the timeline, the default is false.")
    private Boolean acrossTimeline;

    @Schema(description = """
            The number of rows to be obtained when obtaining multiple rows of data,
            empty to be limited by the system's MAX_BATCH_SIZE.""")
    private Integer limitSize;
}
