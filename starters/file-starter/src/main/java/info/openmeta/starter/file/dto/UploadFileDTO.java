package info.openmeta.starter.file.dto;

import info.openmeta.framework.orm.enums.FileType;
import info.openmeta.starter.file.enums.FileSource;
import lombok.Data;

import java.io.InputStream;
import java.io.Serializable;

/**
 * The DTO of upload file.
 */
@Data
public class UploadFileDTO {

    private String modelName;

    private Serializable rowId;

    // Simple file name without extension
    private String fileName;

    private FileType fileType;

    // The file size in KB
    private int fileSize;

    private FileSource fileSource;

    private InputStream inputStream;
}
