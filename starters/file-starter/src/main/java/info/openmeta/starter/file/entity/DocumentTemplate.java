package info.openmeta.starter.file.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * DocumentTemplate Model
 */
@Data
@Schema(name = "DocumentTemplate")
@EqualsAndHashCode(callSuper = true)
public class DocumentTemplate extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Tenant ID")
    private String tenantId;

    @Schema(description = "Model Name")
    private String modelName;

    @Schema(description = "File Name")
    private String fileName;

    @Schema(description = "File Template")
    private Long fileId;

    @Schema(description = "Convert To PDF")
    private Boolean convertToPdf;

    @Schema(description = "Disabled")
    private Boolean disabled;
}