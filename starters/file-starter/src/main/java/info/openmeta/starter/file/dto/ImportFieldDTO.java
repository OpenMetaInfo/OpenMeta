package info.openmeta.starter.file.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@Schema(name = "ImportFieldDTO")
public class ImportFieldDTO {

    @Schema(description = "Excel header title")
    private String header;

    @Schema(description = "Import Rule")
    private String fieldName;

    @Schema(description = "Required field")
    private Boolean required;
}
