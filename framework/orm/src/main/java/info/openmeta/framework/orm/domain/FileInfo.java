package info.openmeta.framework.orm.domain;

import info.openmeta.framework.orm.enums.FileType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(name = "FileInfo")
public class FileInfo {

    @Schema(description = "File ID")
    private String fileId;

    @Schema(description = "File Name")
    private String fileName;

    @Schema(description = "File Type")
    private FileType fileType;

    @Schema(description = "Temporary Download URL")
    private String url;

    @Schema(description = "File Size (KB)")
    private Integer size;

    @Schema(description = "Checksum")
    private String checksum;

}
