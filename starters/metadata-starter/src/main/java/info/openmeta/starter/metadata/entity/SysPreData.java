package info.openmeta.starter.metadata.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * SysPreData Model
 */
@Data
@Schema(name = "SysPreData")
@EqualsAndHashCode(callSuper = true)
public class SysPreData extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Model Name")
    private String model;

    @Schema(description = "Predefined ID")
    private String preId;

    @Schema(description = "Row Data ID")
    private Long rowId;

    @Schema(description = "Updatable")
    private Boolean updatable;

    @Schema(description = "Disabled")
    private Boolean disabled;
}