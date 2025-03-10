package info.openmeta.starter.file.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.starter.file.enums.ImportStatus;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * ImportHistory Model
 */
@Data
@Schema(name = "ImportHistory")
@EqualsAndHashCode(callSuper = true)
public class ImportHistory extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Tenant ID")
    private String tenantId;

    @Schema(description = "Template ID")
    private Long templateId;

    @Schema(description = "Original File ID")
    private String originalFileId;

    @Schema(description = "File Name")
    private String fileName;

    @Schema(description = "Import Status")
    private ImportStatus status;

    @Schema(description = "Failed File ID")
    private String failedFileId;

    @Schema(description = "Total Rows")
    private Integer totalRows;

    @Schema(description = "Success Rows")
    private Integer successRows;

    @Schema(description = "Failed Rows")
    private Integer failedRows;

    @Schema(description = "Deleted")
    private Boolean deleted;
}