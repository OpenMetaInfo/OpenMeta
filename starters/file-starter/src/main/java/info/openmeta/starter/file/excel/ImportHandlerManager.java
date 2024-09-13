package info.openmeta.starter.file.excel;

import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.service.ModelService;
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
     * @param modelName  The model name
     * @param fieldNames The field names
     * @param rows       The rows
     * @param importRule The import rule
     * @return the failed rows cannot be imported
     */
    public List<Map<String, Object>> importData(String modelName, List<String> fieldNames,
                                                List<Map<String, Object>> rows, ImportRule importRule) {
        // convert data
        List<BaseImportHandler> handlers = new ArrayList<>();
        for (String fieldName : fieldNames) {
            if (ModelManager.existField(modelName, fieldName)) {
                MetaField metaField = ModelManager.getModelField(modelName, fieldName);
                BaseImportHandler handler = this.createHandler(metaField);
                if (handler != null) {
                    handlers.add(handler);
                }
            }
        }
        for (BaseImportHandler handler : handlers) {
            handler.handleRows(rows);
        }
        this.createOrUpdate(modelName, rows, importRule);
        return new ArrayList<>();
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

    private void createOrUpdate(String modelName, List<Map<String, Object>> rows, ImportRule importRule) {
        if (ImportRule.ONLY_CREATE.equals(importRule)) {
            modelService.createList(modelName, rows);
        } else if (ImportRule.ONLY_UPDATE.equals(importRule)) {
            modelService.updateList(modelName, rows);
        } else {
            modelService.createList(modelName, rows);
        }
    }
}
