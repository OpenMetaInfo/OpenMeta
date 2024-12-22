package info.openmeta.starter.file.excel;

import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.SpringContextUtils;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.constant.ModelConstant;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.starter.file.constant.FileConstant;
import info.openmeta.starter.file.dto.ImportDataDTO;
import info.openmeta.starter.file.dto.ImportFieldDTO;
import info.openmeta.starter.file.dto.ImportTemplateDTO;
import info.openmeta.starter.file.enums.ImportRule;
import info.openmeta.starter.file.excel.handler.*;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.*;

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
        this.executeCustomHandler(importTemplateDTO.getCustomHandler(), importDataDTO);
        // separate failed rows
        this.separateFailedRows(importDataDTO);
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
     * @param importDataDTO The import data DTO
     */
    private void executeCustomHandler(String handlerName, ImportDataDTO importDataDTO) {
        if (StringUtils.hasText(handlerName)) {
            if (!StringTools.isBeanName(handlerName)) {
                throw new IllegalArgumentException("The name of custom import handler `{0}` is invalid.", handlerName);
            }
            try {
                CustomImportHandler handler = SpringContextUtils.getBean(handlerName, CustomImportHandler.class);
                handler.handleImportData(importDataDTO.getRows(), importDataDTO.getEnv());
            } catch (NoSuchBeanDefinitionException e) {
                throw new IllegalArgumentException("The custom import handler `{0}` is not found.", handlerName);
            }
        }
    }

    /**
     * Separates failed rows from the 'rows' field in the ImportDataDTO object
     * and moves them to the 'failedRows' field based on the presence of the key 'Failed Reason'.
     *
     * @param importDataDTO The ImportDataDTO object to process
     */
    public void separateFailedRows(ImportDataDTO importDataDTO) {
        List<Map<String, Object>> failedRows = new ArrayList<>();
        // Use an iterator to traverse the list to avoid ConcurrentModificationException
        Iterator<Map<String, Object>> rowIterator = importDataDTO.getRows().iterator();
        Iterator<Map<String, Object>> originalRowIterator = importDataDTO.getOriginalRows().iterator();
        // Traverse the rows and originalRows list simultaneously
        while (rowIterator.hasNext() && originalRowIterator.hasNext()) {
            Map<String, Object> row = rowIterator.next();
            Map<String, Object> originalRow = originalRowIterator.next();
            if (row.containsKey(FileConstant.FAILED_REASON)) {
                originalRow.put(FileConstant.FAILED_REASON, row.get(FileConstant.FAILED_REASON));
                // Remove row containing "Failed Reason" from rows and add them to failedRows
                rowIterator.remove();
                originalRowIterator.remove();
                // Add the original row to the failed rows
                failedRows.add(originalRow);
            }
        }
        importDataDTO.setFailedRows(failedRows);
    }

    /**
     * Create or update the data by the import rule
     *
     * @param importTemplateDTO The importTemplateDTO object
     * @param rows       The rows
     */
    private void persistToDatabase(ImportTemplateDTO importTemplateDTO, List<Map<String, Object>> rows) {
        if (CollectionUtils.isEmpty(rows)) {
            return;
        }
        ImportRule importRule = importTemplateDTO.getImportRule();
        if (ImportRule.CREATE_OR_UPDATE.equals(importRule)) {
            this.createOrUpdate(importTemplateDTO, rows, false);
        } else if (ImportRule.ONLY_CREATE.equals(importRule)) {
            modelService.createList(importTemplateDTO.getModelName(), rows);
        } else if (ImportRule.ONLY_UPDATE.equals(importRule)) {
            this.createOrUpdate(importTemplateDTO, rows, true);
        }
    }

    /**
     * Create or update the data
     *
     * @param importTemplateDTO The model name
     * @param rows      The rows
     * @param onlyUpdate The only update flag
     */
    private void createOrUpdate(ImportTemplateDTO importTemplateDTO, List<Map<String, Object>> rows, boolean onlyUpdate) {
        String modelName = importTemplateDTO.getModelName();
        List<String> uniqueConstraints = importTemplateDTO.getUniqueConstraints();
        List<Map<String, Object>> createDataList = new ArrayList<>();
        List<Map<String, Object>> updateDataList = new ArrayList<>();

        // Step 1: Get the unique key to row map
        Map<String, Map<String, Object>> rowKeyMap = new HashMap<>();
        Map<String, Set<Object>> uniqueValuesMap = new HashMap<>();
        for (String field : uniqueConstraints) {
            uniqueValuesMap.put(field, new HashSet<>());
        }
        for (Map<String, Object> row : rows) {
            uniqueValuesMap.forEach((k, v) -> v.add(row.get(k)));
            String key = this.generateUniqueKey(row, uniqueConstraints);
            Assert.notTrue(rowKeyMap.containsKey(key),
                    "The unique key `{0}` of uniqueConstraints `{1}` is duplicated in uploaded file `{2}`.",
                    key, importTemplateDTO.getUniqueConstraints(), importTemplateDTO.getFileName());
            rowKeyMap.put(key, row);
        }

        // Step 2: Get the row key map from the database by the values of unique constraints
        Map<String, Map<String, Object>> dbRowKeyMap = this.getRowKeyMapFromDB(importTemplateDTO, uniqueValuesMap);

        // Step 3: Compare the row key map from the database and the import data, to get the data to be created and updated
        List<String> newKeys = new ArrayList<>();
        for (Map.Entry<String, Map<String, Object>> entry : rowKeyMap.entrySet()) {
            String key = entry.getKey();
            Map<String, Object> row = entry.getValue();
            if (dbRowKeyMap.containsKey(key)) {
                row.put(ModelConstant.ID, dbRowKeyMap.get(key).get(ModelConstant.ID));
                updateDataList.add(row);
            } else {
                createDataList.add(row);
                newKeys.add(key);
            }
        }
        // Step 4: Execute the create or update operation
        if (onlyUpdate && !createDataList.isEmpty()) {
            throw new IllegalArgumentException("In update-only mode, the following data does not exist:\n {0}", newKeys);
        }
        if (!updateDataList.isEmpty()) {
            modelService.updateList(modelName, updateDataList);
        }
        if (!createDataList.isEmpty()) {
            modelService.createList(modelName, createDataList);
        }
    }

    /**
     * Get the row key map from the database
     *
     * @param importTemplateDTO The import template DTO
     * @param uniqueValuesMap The unique values map
     * @return The row key map
     */
    private Map<String, Map<String, Object>> getRowKeyMapFromDB(ImportTemplateDTO importTemplateDTO, Map<String, Set<Object>> uniqueValuesMap) {
        Filters filters = Filters.and(uniqueValuesMap.entrySet().stream()
                .map(e -> new Filters().in(e.getKey(), e.getValue()))
                .toList());
        List<String> fields = new ArrayList<>(List.of(ModelConstant.ID));
        fields.addAll(importTemplateDTO.getUniqueConstraints());
        FlexQuery flexQuery = new FlexQuery(fields, filters);
        List<Map<String, Object>> dbRows = modelService.searchList(importTemplateDTO.getModelName(), flexQuery);
        Map<String, Map<String, Object>> dbRowKeyMap = new HashMap<>();
        for (Map<String, Object> dbRow : dbRows) {
            String key = generateUniqueKey(dbRow, importTemplateDTO.getUniqueConstraints());
            dbRowKeyMap.put(key, dbRow);
        }
        return dbRowKeyMap;
    }

    /**
     * Generate the unique key
     *
     * @param data The data
     * @param uniqueConstraints The unique constraints
     * @return The unique key
     */
    private String generateUniqueKey(Map<String, Object> data, List<String> uniqueConstraints) {
        StringBuilder keyBuilder = new StringBuilder();
        for (String field : uniqueConstraints) {
            Object value = data.get(field);
            keyBuilder.append(value != null ? value.toString() : "").append(" && ");
        }
        return keyBuilder.toString();
    }
}
