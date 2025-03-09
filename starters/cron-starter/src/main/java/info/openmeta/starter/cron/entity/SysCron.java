package info.openmeta.starter.cron.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.time.LocalDateTime;

/**
 * SysCron Model
 */
@Data
@Schema(name = "SysCron")
@EqualsAndHashCode(callSuper = true)
public class SysCron extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Cron Job Name")
    private String name;

    @Schema(description = "Cron Expression")
    private String cronExpression;

    @Schema(description = "Semantic Description")
    private String cronSemantic;

    @Schema(description = "Limit the Execution Times")
    private Boolean limitExecution;

    @Schema(description = "Remaining Execution Times")
    private Integer remainingCount;

    @Schema(description = "Next Execution Time")
    private LocalDateTime nextExecTime;

    @Schema(description = "Last Execution Time")
    private LocalDateTime lastExecTime;

    @Schema(description = "Redo Missed Task")
    private Boolean redoMisfire;

    @Schema(description = "Priority")
    private Integer priority;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Active")
    private Boolean active;
}