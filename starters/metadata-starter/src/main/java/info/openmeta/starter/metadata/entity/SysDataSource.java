package info.openmeta.starter.metadata.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * SysDataSource Model
 * The default datasource is configured in the spring.datasource configuration.
This model is used to configure additional external data sources.
 */
@Data
@Schema(name = "SysDataSource")
@EqualsAndHashCode(callSuper = true)
public class SysDataSource extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Data Source Name")
    private String name;

    @Schema(description = "Data Source Key")
    private String key;

    @Schema(description = "JDBC URL")
    private String jdbcUrl;

    @Schema(description = "Username")
    private String username;

    @Schema(description = "Password")
    private String password;

    @Schema(description = "Initial Pool Size")
    private Integer initialSize;

    @Schema(description = "Maximum Pool Size")
    private Integer maxActive;

    @Schema(description = "Minimum Idle")
    private Integer minIdle;

    @Schema(description = "Connection Timeout")
    private Integer maxWait;

    @Schema(description = "Readonly")
    private Boolean readonly;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Disabled")
    private Boolean disabled;
}