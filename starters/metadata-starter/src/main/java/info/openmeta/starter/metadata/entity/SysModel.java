package info.openmeta.starter.metadata.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.framework.orm.enums.IdStrategy;
import info.openmeta.framework.orm.enums.StorageType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.util.List;

/**
 * SysModel Model
 */
@Data
@Schema(name = "SysModel")
@EqualsAndHashCode(callSuper = true)
public class SysModel extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    protected Long id;

    @Schema(description = "App ID")
    private Long appId;

    @Schema(description = "Label Name")
    private String labelName;

    @Schema(description = "Model Name")
    private String modelName;

    @Schema(description = "Enable Soft Delete")
    private Boolean softDelete;

    @Schema(description = "Default Order")
    private String defaultOrder;

    @Schema(description = "Display Name")
    private List<String> displayName;

    @Schema(description = "Search Name")
    private List<String> searchName;

    @Schema(description = "Table Name")
    private String tableName;

    @Schema(description = "Is Timeline Model")
    private Boolean timeline;

    @Schema(description = "ID Strategy")
    private IdStrategy idStrategy;

    @Schema(description = "Storage Type")
    private StorageType storageType;

    @Schema(description = "Enable Version Lock")
    private Boolean versionLock;

    @Schema(description = "Enable Multi-tenancy")
    private Boolean multiTenant;

    @Schema(description = "Partition Field")
    private String partitionField;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Model Fields")
    private List<SysField> modelFields;

    @Schema(description = "Disabled")
    private Boolean disabled;
}