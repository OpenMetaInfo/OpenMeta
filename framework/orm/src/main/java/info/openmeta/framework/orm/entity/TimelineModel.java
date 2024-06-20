package info.openmeta.framework.orm.entity;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.time.LocalDate;

/**
 * Abstract class of timeline model, with sliceId, effectiveStart and effectiveEnd fields.
 */
@Data
@Schema(name = "TimelineModel")
@EqualsAndHashCode(callSuper = true)
public abstract class TimelineModel extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Slice ID")
    private Long sliceId;

    @Schema(description = "Effective start date")
    private LocalDate effectiveStart;

    @Schema(description = "Effective end date")
    private LocalDate effectiveEnd;
}
