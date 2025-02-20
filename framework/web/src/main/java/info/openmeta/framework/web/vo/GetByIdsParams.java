package info.openmeta.framework.web.vo;

import info.openmeta.framework.orm.domain.SubQuery;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;

/**
 * GetByIdsParams for /getByIds API.
 */
@Data
@Schema(name = "GetByIdsParams")
public class GetByIdsParams {

    @Schema(description = "Data IDs, number or string type.", type = "array", requiredMode = Schema.RequiredMode.REQUIRED)
    private List<Serializable> ids;

    @Schema(description = "Field names. If not specified, it defaults to all visible fields.", example = "[]")
    private List<String> fields;

    @Schema(description = "SubQuery parameters for relational fields.")
    private Map<String, SubQuery> subQueries;

    @Schema(description = "Effective date for timeline model, default is `Today`.")
    private LocalDate effectiveDate;

}
