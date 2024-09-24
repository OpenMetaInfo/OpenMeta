package info.openmeta.starter.file.dto;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * The DTO of import data.
 */
@Data
@NoArgsConstructor
public class ImportDataDTO {

    private List<Map<String, Object>> rows;

    private List<Map<String, Object>> failedRows;

    private Map<String, Object> env;

    private List<Map<String, Object>> originalRows;
}
