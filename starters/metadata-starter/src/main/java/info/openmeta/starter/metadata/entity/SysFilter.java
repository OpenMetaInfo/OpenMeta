package info.openmeta.starter.metadata.entity;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * SysFilter Model
 */
@Data
@Schema(name = "SysFilter")
@EqualsAndHashCode(callSuper = true)
public class SysFilter extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Filter Name")
    private String name;

    @Schema(description = "Filter Conditions")
    private Filters filters;

    @Schema(description = "Model Name")
    private String model;

    @Schema(description = "Query Text")
    private String query;

    @Schema(description = "Disabled")
    private Boolean disabled;
}