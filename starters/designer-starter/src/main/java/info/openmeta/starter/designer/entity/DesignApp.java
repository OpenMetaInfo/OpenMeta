package info.openmeta.starter.designer.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.framework.orm.enums.DatabaseType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * DesignApp Model
 */
@Data
@Schema(name = "DesignApp")
@EqualsAndHashCode(callSuper = true)
public class DesignApp extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

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

    @Schema(description = "Package Name")
    private String packageName;

    @Schema(description = "Deleted")
    private Boolean deleted;
}