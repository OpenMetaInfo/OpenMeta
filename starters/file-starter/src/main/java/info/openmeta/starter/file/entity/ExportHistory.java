package info.openmeta.starter.file.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * ExportHistory Model
 */
@Data
@Schema(name = "ExportHistory")
@EqualsAndHashCode(callSuper = true)
public class ExportHistory extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Tenant ID")
    private String tenantId;

    @Schema(description = "Template ID")
    private Long templateId;

    @Schema(description = "Exported File ID")
    private String exportedFileId;

    @Schema(description = "Deleted")
    private Boolean deleted;
}