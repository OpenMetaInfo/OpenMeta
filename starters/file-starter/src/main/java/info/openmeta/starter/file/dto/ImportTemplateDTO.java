package info.openmeta.starter.file.dto;

import info.openmeta.starter.file.enums.ImportRule;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

@Data
public class ImportTemplateDTO {

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

    @Schema(description = "Custom Handler")
    private String customHandler;

    @Schema(description = "Import Field DTOs")
    private List<ImportFieldDTO> importFields;

    public void addImportField(ImportFieldDTO importFieldDTO) {
        if (CollectionUtils.isEmpty(importFields)) {
            this.importFields = new ArrayList<>();
            this.importFields.add(importFieldDTO);
        }
    }
}