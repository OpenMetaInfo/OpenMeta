package info.openmeta.framework.web.dto;

import info.openmeta.framework.orm.enums.FileType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class FileInfo {

    private String fileName;

    private FileType fileType;

    private String content;

}
