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

    @Schema(description = "Template ID")
    private Long templateId;

    @Schema(description = "File ID")
    private Long fileId;

    @Schema(description = "File Name")
    private String fileName;

    @Schema(description = "Import Status")
    private ImportStatus status;

    @Schema(description = "Failed File ID")
    private Long failedFileId;

    @Schema(description = "Total Rows")
    private Integer totalRows;

    @Schema(description = "Success Rows")
    private Integer successRows;

    @Schema(description = "Failed Rows")
    private Integer failedRows;

    @Schema(description = "Disabled")
    private Boolean disabled;
}