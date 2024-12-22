package info.openmeta.starter.file.excel.handler;

import java.util.List;
import java.util.Map;

/**
 * CustomExportHandler for custom business logic
 */
public interface CustomExportHandler {

    /**
     * Handle the export data
     *
     * @param rows The export data rows
     */
    void handleExportData(List<Map<String, Object>> rows);
}
