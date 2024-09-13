package info.openmeta.starter.file.dto;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * The DTO of import data.
 */
@Data
@NoArgsConstructor
public class ImportDataDTO {

    private List<String> fields;

    private Set<String> requiredFields;

    private List<Map<String, Object>> rows;

    private List<Map<String, Object>> failedRows;

    private List<String> headers;

    /**
     * Constructor
     *
     * @param fields         The fields
     * @param requiredFields The required fields
     * @param rows           The rows
     */
    public ImportDataDTO(List<String> fields, Set<String> requiredFields, List<Map<String, Object>> rows) {
        this.fields = fields;
        this.requiredFields = requiredFields;
        this.rows = rows;
    }
}
