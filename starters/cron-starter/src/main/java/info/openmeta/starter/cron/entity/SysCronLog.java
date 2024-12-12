package info.openmeta.starter.cron.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.starter.cron.enums.CronStatus;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.time.LocalDateTime;

/**
 * SysCronLog Model
 */
@Data
@Schema(name = "SysCronLog")
@EqualsAndHashCode(callSuper = true)
public class SysCronLog extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Cron ID")
    private Long cronId;

    @Schema(description = "Cron Job Name")
    private String cronName;

    @Schema(description = "Cron Execution State")
    private CronStatus status;

    @Schema(description = "Execution Start Time")
    private LocalDateTime startTime;

    @Schema(description = "Execution End Time")
    private LocalDateTime endTime;

    @Schema(description = "Total Execution Time (s)")
    private Double totalTime;

    @Schema(description = "Error Message")
    private String errorMessage;

    @Schema(description = "Disabled")
    private Boolean disabled;
}