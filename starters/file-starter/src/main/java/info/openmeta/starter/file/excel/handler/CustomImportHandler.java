package info.openmeta.starter.file.excel.handler;

import info.openmeta.starter.file.dto.ImportDataDTO;

/**
 * CustomImportHandler for custom business logic
 */
public interface CustomImportHandler {

    /**
     * Handle the import data
     *
     * @param importDataDTO The import data DTO
     */
    void handleImportData(ImportDataDTO importDataDTO);
}
