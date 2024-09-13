package info.openmeta.starter.file.vo;

import com.fasterxml.jackson.core.type.TypeReference;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.web.utils.FileUtils;
import info.openmeta.starter.file.dto.ImportFieldDTO;
import info.openmeta.starter.file.enums.ImportRule;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

@Data
@Schema(name = "ImportWizard")
public class ImportWizard {

    @Schema(description = "Model name", hidden = true)
    private String modelName;

    @Schema(description = "Uploaded file")
    private MultipartFile file;

    @Schema(description = "Uploaded file name", hidden = true)
    private String fileName;

    @Schema(description = "Import Rule")
    private ImportRule importRule;

    @Schema(description = "Number of header rows", defaultValue = "1")
    private int headerRows = 1;

    @Schema(description = "Unique Constraints")
    private String uniqueConstraints;

    @Schema(description = """
            JSON string of the import fields info. e.g.
                [{"header": "Product Code", "fieldName": "productCode", "required": true},
                 {"header": "Product Name", "fieldName": "productName", "required": true}]""")
    private String importFieldStr;

    @Schema(hidden = true)
    private List<ImportFieldDTO> importFieldDTOList;

    @Schema(description = "Whether to ignore empty values")
    private Boolean ignoreEmpty;

    @Schema(description = "Whether to continue importing next row data when encountering error.")
    private Boolean skipException;

    /**
     * Set the uploaded file and extract the file name.
     *
     * @param file the uploaded file
     */
    private void setFile(MultipartFile file) {
        this.file = file;
        this.fileName = FileUtils.getShortFileName(file);
    }

    /**
     * Set the importFieldStr and parse it into a list of ImportFieldDTO.
     *
     * @param importFieldStr the JSON string of the ImportFieldDTO list
     */
    public void setImportFieldStr(String importFieldStr) {
        this.importFieldStr = importFieldStr;
        try {
            TypeReference<List<ImportFieldDTO>> typeRef = new TypeReference<>() {};
            this.importFieldDTOList = JsonMapper.stringToObject(importFieldStr, typeRef);
        } catch (Exception e) {
            throw new IllegalArgumentException("The JSON string of the import fields must be in JSON format: {0}", importFieldStr, e);
        }
    }
}
