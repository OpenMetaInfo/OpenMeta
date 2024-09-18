package info.openmeta.starter.file.excel;

import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.SpringContextUtils;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.starter.file.dto.ImportDataDTO;
import info.openmeta.starter.file.dto.ImportFieldDTO;
import info.openmeta.starter.file.dto.ImportTemplateDTO;
import info.openmeta.starter.file.enums.ImportRule;
import info.openmeta.starter.file.excel.handler.*;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

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
     * @param importTemplateDTO The import config DTO
     * @param importDataDTO  The import data DTO
     */
    public void importData(ImportTemplateDTO importTemplateDTO, ImportDataDTO importDataDTO) {
        // convert data
        String modelName = importTemplateDTO.getModelName();
        List<BaseImportHandler> handlers = new ArrayList<>();
        for (ImportFieldDTO importFieldDTO : importTemplateDTO.getImportFields()) {
            if (ModelManager.existField(modelName, importFieldDTO.getFieldName())) {
                MetaField metaField = ModelManager.getModelField(modelName, importFieldDTO.getFieldName());
                if (!Boolean.TRUE.equals(importFieldDTO.getRequired())) {
                    importFieldDTO.setRequired(metaField.isRequired());
                }
                BaseImportHandler handler = this.createHandler(metaField, importFieldDTO);
                handlers.add(handler);
            }
        }
        // execute standard handlers
        for (BaseImportHandler handler : handlers) {
            handler.handleRows(importDataDTO.getRows());
        }
        // execute custom handler
        this.executeCustomHandler(importTemplateDTO.getCustomHandler(), importDataDTO.getRows());
        // persist to database
        this.persistToDatabase(importTemplateDTO, importDataDTO.getRows());
    }

    /**
     * Create the import data handler
     *
     * @param metaField The meta field
     * @return The handler
     */
    private BaseImportHandler createHandler(MetaField metaField, ImportFieldDTO importFieldDTO) {
        return switch (metaField.getFieldType()) {
            case BOOLEAN -> new BooleanHandler(metaField, importFieldDTO);
            case DATE -> new DateHandler(metaField, importFieldDTO);
            case DATE_TIME -> new DateTimeHandler(metaField, importFieldDTO);
            case MULTI_OPTION -> new MultiOptionHandler(metaField, importFieldDTO);
            case OPTION -> new OptionHandler(metaField, importFieldDTO);
            default -> new DefaultHandler(metaField, importFieldDTO);
        };
    }

    /**
     * Execute the custom handler
     *
     * @param handlerName The handler name
     * @param rows        The rows
     */
    private void executeCustomHandler(String handlerName, List<Map<String, Object>> rows) {
        if (StringUtils.hasText(handlerName)) {
            if (!StringTools.isModelName(handlerName)) {
                throw new IllegalArgumentException("The name of custom handler `{0}` is invalid.", handlerName);
            }
            try {
                CustomHandler handler = SpringContextUtils.getBean(handlerName, CustomHandler.class);
                handler.handleRows(rows);
            } catch (NoSuchBeanDefinitionException e) {
                throw new IllegalArgumentException("The custom handler `{0}` is not found.", handlerName);
            }
        }
    }

    /**
     * Create or update the data by the import rule
     *
     * @param importTemplateDTO The importTemplateDTO object
     * @param rows       The rows
     */
    private void persistToDatabase(ImportTemplateDTO importTemplateDTO, List<Map<String, Object>> rows) {
        String modelName = importTemplateDTO.getModelName();
        ImportRule importRule = importTemplateDTO.getImportRule();
        if (ImportRule.CREATE_OR_UPDATE.equals(importRule)) {
            this.createOrUpdate(modelName, importTemplateDTO.getUniqueConstraints(), rows);
        } else if (ImportRule.ONLY_CREATE.equals(importRule)) {
            modelService.createList(modelName, rows);
        } else if (ImportRule.ONLY_UPDATE.equals(importRule)) {
            this.update(modelName, rows);
        }
    }

    /**
     * Create or update the data
     *
     * @param modelName The model name
     * @param uniqueConstraints The unique constraints
     * @param rows      The rows
     */
    private void createOrUpdate(String modelName, List<String> uniqueConstraints, List<Map<String, Object>> rows) {
        List<Map<String, Object>> tempRows = new ArrayList<>();
        modelService.createList(modelName, rows);
    }

    /**
     * Update the data
     *
     * @param modelName The model name
     * @param rows      The rows
     */
    private void update(String modelName, List<Map<String, Object>> rows) {
        modelService.updateList(modelName, rows);
    }
}
