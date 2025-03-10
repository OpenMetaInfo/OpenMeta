package info.openmeta.starter.file.entity;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * ExportTemplate Model
 */
@Data
@Schema(name = "ExportTemplate")
@EqualsAndHashCode(callSuper = true)
public class ExportTemplate extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Tenant ID")
    private String tenantId;

    @Schema(description = "File Name")
    private String fileName;

    @Schema(description = "Sheet Name")
    private String sheetName;

    @Schema(description = "Model Name")
    private String modelName;

    @Schema(description = "File Template ID")
    private String fileId;

    @Schema(description = "Filters")
    private Filters filters;

    @Schema(description = "Orders")
    private Orders orders;

    @Schema(description = "Custom Export Handler")
    private String customHandler;

    @Schema(description = "Enable Transpose")
    private Boolean enableTranspose;
}