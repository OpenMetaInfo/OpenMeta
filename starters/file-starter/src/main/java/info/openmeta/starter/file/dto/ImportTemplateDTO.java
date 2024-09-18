package info.openmeta.starter.file.dto;

import info.openmeta.starter.file.enums.ImportRule;
import lombok.Data;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Data
public class ImportTemplateDTO {

    private String modelName;

    private ImportRule importRule;

    private List<String> uniqueConstraints;

    private Boolean ignoreEmpty;

    private Boolean skipException;

    private String customHandler;

    private Map<String, Object> env;

    private List<ImportFieldDTO> importFields;

    public void addImportField(ImportFieldDTO importFieldDTO) {
        if (CollectionUtils.isEmpty(importFields)) {
            this.importFields = new ArrayList<>();
        }
        this.importFields.add(importFieldDTO);
    }
}