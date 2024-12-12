package info.openmeta.starter.metadata.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * SysViewDefault Model
 */
@Data
@Schema(name = "SysViewDefault")
@EqualsAndHashCode(callSuper = true)
public class SysViewDefault extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "View ID")
    private Long viewId;

    @Schema(description = "View Code")
    private String viewCode;

    @Schema(description = "Navigation ID")
    private Long navId;

    @Schema(description = "Model Name")
    private String modelName;

    @Schema(description = "Disabled")
    private Boolean disabled;
}