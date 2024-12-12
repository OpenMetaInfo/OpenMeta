package info.openmeta.starter.file.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * ExportTemplateField Model
 */
@Data
@Schema(name = "ExportTemplateField")
@EqualsAndHashCode(callSuper = true)
public class ExportTemplateField extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    protected Long id;

    @Schema(description = "Export Template ID")
    private Long templateId;

    @Schema(description = "Field Name")
    private String fieldName;

    @Schema(description = "Custom Header")
    private String customHeader;

    @Schema(description = "Sequence")
    private Integer sequence;

    @Schema(description = "Disabled")
    private Boolean disabled;
}