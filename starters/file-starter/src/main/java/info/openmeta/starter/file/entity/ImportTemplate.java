package info.openmeta.starter.file.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.starter.file.enums.ImportRule;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.util.List;

/**
 * ImportTemplate Model
 */
@Data
@Schema(name = "ImportTemplate")
@EqualsAndHashCode(callSuper = true)
public class ImportTemplate extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Tenant ID")
    private String tenantId;

    @Schema(description = "Template Name")
    private String name;

    @Schema(description = "Model Name")
    private String modelName;

    @Schema(description = "Import Rule")
    private ImportRule importRule;

    @Schema(description = "Unique Constraints")
    private List<String> uniqueConstraints;

    @Schema(description = "Ignore Empty Value")
    private Boolean ignoreEmpty;

    @Schema(description = "Skip Abnormal Data")
    private Boolean skipException;

    @Schema(description = "Custom Import Handler")
    private String customHandler;

    @Schema(description = "Synchronous Import")
    private Boolean syncImport;

    @Schema(description = "Include Import Description")
    private Boolean includeDescription;

    @Schema(description = "Description")
    private String description;

    @Schema(description = "Import Field List")
    private List<ImportTemplateField> importFields;
}