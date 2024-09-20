package info.openmeta.starter.file.excel.handler;

import java.util.List;
import java.util.Map;

/**
 * CustomImportHandler for custom business logic
 */
public interface CustomImportHandler {

    /**
     * Handle the import data
     *
     * @param rows The import data rows
     * @param env The environment variables
     */
    void handleImportData(List<Map<String, Object>> rows, Map<String, Object> env);
}
