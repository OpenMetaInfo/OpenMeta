package info.openmeta.framework.orm.domain;

import info.openmeta.framework.orm.enums.FileType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(name = "FileObject")
public class FileObject {

    @Schema(description = "File Name")
    private String fileName;

    @Schema(description = "File Type")
    private FileType fileType;

    @Schema(description = "File Content")
    private String content;

}
