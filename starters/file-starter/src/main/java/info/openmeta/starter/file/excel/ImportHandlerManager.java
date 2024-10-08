package info.openmeta.starter.file.excel;

import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.starter.file.dto.ImportConfigDTO;
import info.openmeta.starter.file.dto.ImportDataDTO;
import info.openmeta.starter.file.enums.ImportRule;
import info.openmeta.starter.file.excel.handler.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * ImportHandlerManager
 */
@Component
public class ImportHandlerManager {

    @Autowired
    private ModelService<?> modelService;

    /**
     * Import data
     *
     * @param importConfigDTO The import config DTO
     * @param importDataDTO  The import data DTO
     */
    public void importData(ImportConfigDTO importConfigDTO, ImportDataDTO importDataDTO) {
        // convert data
        String modelName = importConfigDTO.getModelName();
        List<BaseImportHandler> handlers = new ArrayList<>();
        for (String fieldName : importDataDTO.getFields()) {
            if (ModelManager.existField(modelName, fieldName)) {
                MetaField metaField = ModelManager.getModelField(modelName, fieldName);
                BaseImportHandler handler = this.createHandler(metaField);
                if (handler != null) {
                    handlers.add(handler);
                }
            }
        }
        for (BaseImportHandler handler : handlers) {
            handler.handleRows(importDataDTO.getRows());
        }
        this.createOrUpdate(importConfigDTO, importDataDTO.getRows());
    }

    /**
     * Create the import data handler
     *
     * @param metaField The meta field
     * @return The handler
     */
    private BaseImportHandler createHandler(MetaField metaField) {
        return switch (metaField.getFieldType()) {
            case BOOLEAN -> new BooleanHandler(metaField);
            case DATE -> new DateHandler(metaField);
            case DATE_TIME -> new DateTimeHandler(metaField);
            case MULTI_OPTION -> new MultiOptionHandler(metaField);
            case OPTION -> new OptionHandler(metaField);
            default -> null;
        };
    }

    /**
     * Create or update the data by the import rule
     *
     * @param importConfigDTO The importConfigDTO object
     * @param rows       The rows
     */
    private void createOrUpdate(ImportConfigDTO importConfigDTO, List<Map<String, Object>> rows) {
        String modelName = importConfigDTO.getModelName();
        ImportRule importRule = importConfigDTO.getImportRule();
        if (ImportRule.CREATE_OR_UPDATE.equals(importRule)) {
            modelService.createList(modelName, rows);
        } else if (ImportRule.ONLY_CREATE.equals(importRule)) {
            modelService.createList(modelName, rows);
        } else if (ImportRule.ONLY_UPDATE.equals(importRule)) {
            modelService.updateList(modelName, rows);
        }
    }
}
