package info.openmeta.starter.file.excel.handler;

import java.util.List;
import java.util.Map;

/**
 * CustomHandler for custom business logic
 */
public interface CustomHandler {

    /**
     * Handle the rows
     *
     * @param rows The rows
     */
    void handleRows(List<Map<String, Object>> rows);
}
