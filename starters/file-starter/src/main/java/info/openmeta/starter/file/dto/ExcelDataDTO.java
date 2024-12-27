package info.openmeta.starter.file.dto;

import lombok.Data;

import java.util.List;

/**
 * The DTO of Excel data.
 */
@Data
public class ExcelDataDTO {
    private String fileName;

    private String sheetName;

    private List<String> headers;

    private List<String> fetchFields;

    // Ignore fields in the export template
    private List<String> ignoreFields;

    private List<List<Object>> rowsTable;
}
