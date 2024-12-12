package info.openmeta.starter.metadata.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.framework.orm.enums.DatabaseType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * SysApp Model
 */
@Data
@Schema(name = "SysApp")
@EqualsAndHashCode(callSuper = true)
public class SysApp extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    protected Long id;

    @Schema(description = "App Name")
    private String name;

    @Schema(description = "App Code")
    private String appCode;

    @Schema(description = "App Type")
    private String appType;

    @Schema(description = "Database Type")
    private DatabaseType databaseType;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Disabled")
    private Boolean disabled;
}