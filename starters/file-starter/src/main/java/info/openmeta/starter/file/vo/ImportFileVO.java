package info.openmeta.starter.file.vo;

import com.fasterxml.jackson.core.type.TypeReference;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.web.utils.FileUtils;
import info.openmeta.starter.file.enums.ImportRule;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;

import java.util.LinkedHashMap;

@Data
@Schema(name = "ImportFileVO")
public class ImportFileVO {

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

    @Schema(description = "JSON string of header to fieldName map, e.g. {\"Product Code\":\"productCode\",\"Product Name\":\"name\"}")
    private String headerFieldJSON;

    @Schema(description = "Header to fieldName mapping", hidden = true)
    private LinkedHashMap<String, String> headerFieldMap;

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
     * Set the headerFieldJSON and parse it into a LinkedHashMap.
     *
     * @param headerFieldJSON the JSON string of header to fieldName map
     */
    public void setHeaderFieldJSON(String headerFieldJSON) {
        this.headerFieldJSON = headerFieldJSON;
        try {
            TypeReference<LinkedHashMap<String, String>> typeRef = new TypeReference<>() {};
            this.headerFieldMap = JsonMapper.stringToObject(headerFieldJSON, typeRef);
        } catch (Exception e) {
            throw new IllegalArgumentException("Header-field JSON must be in JSON format: {0}", headerFieldJSON, e);
        }
    }
}
