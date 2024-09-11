package info.openmeta.starter.file.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import info.openmeta.framework.orm.enums.FileType;
import info.openmeta.starter.file.enums.FileSource;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * FileRecord Model
 */
@Data
@Schema(name = "FileRecord")
@EqualsAndHashCode(callSuper = true)
public class FileRecord extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "File Name")
    private String fileName;

    @Schema(description = "OSS Key")
    private String ossKey;

    @Schema(description = "File Type")
    private FileType fileType;

    @Schema(description = "File Size(KB)")
    private Integer fileSize;

    @Schema(description = "MimeType")
    private String mimetype;

    @Schema(description = "Checksum")
    private String checksum;

    @Schema(description = "Model Name")
    private String modelName;

    @Schema(description = "Row ID")
    private Long rowId;

    @Schema(description = "Source")
    private FileSource source;

    @Schema(description = "Disabled")
    private Boolean disabled;
}