package info.openmeta.starter.file.service.impl;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.event.AnalysisEventListener;
import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.utils.ListUtils;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.framework.web.utils.FileUtils;
import info.openmeta.starter.file.entity.ImportHistory;
import info.openmeta.starter.file.entity.ImportTemplate;
import info.openmeta.starter.file.entity.ImportTemplateField;
import info.openmeta.starter.file.enums.ImportRule;
import info.openmeta.starter.file.enums.ImportStatus;
import info.openmeta.starter.file.excel.CommonExport;
import info.openmeta.starter.file.excel.ImportHandlerManager;
import info.openmeta.starter.file.service.*;
import info.openmeta.starter.file.vo.ImportFileVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;

@Slf4j
@Service
public class ImportServiceImpl implements ImportService {

    private static final String FAILED_LABEL = "Failed";

    @Autowired
    private FileRecordService fileRecordService;

    @Autowired
    private ImportTemplateService importTemplateService;

    @Autowired
    private ImportTemplateFieldService importTemplateFieldService;

    @Autowired
    private CommonExport commonExport;

    @Autowired
    private ImportHistoryService importHistoryService;

    @Autowired
    private ImportHandlerManager dataHandler;

    public void validateImportTemplate(ImportTemplate importTemplate) {
        Assert.notNull(importTemplate, "Import template cannot be null");
        Assert.notBlank(importTemplate.getModelName(),
                "Import template `{0}` modelName cannot be empty.", importTemplate.getName());
        Assert.notEmpty(importTemplate.getImportFields(),
                "Import template `{0}` fields cannot be empty.", importTemplate.getName());
    }

    /**
     * Get the fileInfo of the import template by template ID
     *
     * @param templateId template ID
     * @return import template fileInfo
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public FileInfo getTemplateFile(Long templateId) {
        ImportTemplate importTemplate = importTemplateService.readOne(templateId);
        validateImportTemplate(importTemplate);
        // Construct the headers order by sequence of the export fields
        Map<String, String> headerToFieldMap = this.extractHeaderFieldMap(importTemplate.getModelName(), templateId);
        List<List<String>> headers = headerToFieldMap.keySet().stream().map(Collections::singletonList).toList();
        // Generate the Excel file
        String fileName = importTemplate.getName();
        String sheetName = importTemplate.getName();
        return commonExport.generateFileAndUpload(fileName, sheetName, headers, null);
    }

    /**
     * Import data from the uploaded file and the import template ID
     *
     * @param templateId       the ID of the import template
     * @param file             the uploaded file
     * @return the import result
     */
    @Override
    public ImportHistory importByTemplate(Long templateId, MultipartFile file) {
        ImportTemplate importTemplate = importTemplateService.readOne(templateId);
        this.validateImportTemplate(importTemplate);
        String modelName = importTemplate.getModelName();
        String fileName = FileUtils.getShortFileName(file);
        Map<String, String> headerToFieldMap = this.extractHeaderFieldMap(modelName, templateId);
        List<String> headers = headerToFieldMap.keySet().stream().toList();
        List<String> fields = headerToFieldMap.values().stream().toList();
        List<Map<String, Object>> allDataList = this.extractDataFromExcel(headerToFieldMap, file);
        ImportRule importRule = importTemplate.getImportRule();
        List<Map<String, Object>> failedDataList = dataHandler.importData(modelName, fields, allDataList, importRule);
        Long fileId = fileRecordService.uploadFile(fileName, file);
        Long failedFileId = null;
        if (!CollectionUtils.isEmpty(failedDataList)) {
            failedFileId = generateFailedExcel(fileName, headers, fields, failedDataList);
        }
        // Generate an export history record
        return this.generateExportHistory(fileName, templateId, fileId, failedFileId);
    }

    /**
     * Import data from the uploaded file and dynamic import settings
     *
     * @return the import result
     */
    @Override
    public ImportHistory importByDynamic(ImportFileVO importFileVO) {
        String modelName = importFileVO.getModelName();
        Map<String, String> headerToFieldMap = importFileVO.getHeaderFieldMap();
        List<String> headers = headerToFieldMap.keySet().stream().toList();
        List<String> fields = headerToFieldMap.values().stream().toList();
        List<Map<String, Object>> allDataList = this.extractDataFromExcel(headerToFieldMap, importFileVO.getFile());
        ImportRule importRule = importFileVO.getImportRule();
        List<Map<String, Object>> failedDataList = dataHandler.importData(modelName, fields, allDataList, importRule);
        String fileName = importFileVO.getFileName();
        Long fileId = fileRecordService.uploadFile(fileName, importFileVO.getFile());
        Long failedFileId = null;
        if (!CollectionUtils.isEmpty(failedDataList)) {
            failedFileId = generateFailedExcel(fileName, headers, fields, failedDataList);
        }
        // Generate an export history record
        return this.generateExportHistory(fileName, null, fileId, failedFileId);
    }

    /**
     * Extract the fields and labels from the export template.
     *
     * @param modelName the model name to be exported
     * @param importTemplateId the ID of the import template
     */
    public Map<String, String> extractHeaderFieldMap(String modelName, Long importTemplateId) {
        Map<String, String> headerToFieldMap = new LinkedHashMap<>();
        // Construct the headers order by sequence of the export fields
        Filters filters = Filters.eq(ImportTemplateField::getTemplateId, importTemplateId);
        Orders orders = Orders.ofAsc(ImportTemplateField::getSequence);
        List<ImportTemplateField> exportFields = importTemplateFieldService.searchList(new FlexQuery(filters, orders));
        exportFields.forEach(exportField -> {
            if (StringUtils.hasText(exportField.getCustomHeader())) {
                headerToFieldMap.put(exportField.getCustomHeader(), exportField.getFieldName());
            } else {
                String labelName = ModelManager.getCascadingFieldLabelName(modelName, exportField.getFieldName());
                headerToFieldMap.put(labelName, exportField.getFieldName());
            }
        });
        return headerToFieldMap;
    }

    /**
     * Extract the fields and labels from the export template.
     *
     * @param headerToFieldMap the mapping of the header fields
     * @param file the uploaded file to be imported
     */
    private List<Map<String, Object>> extractDataFromExcel(Map<String, String> headerToFieldMap, MultipartFile file) {
        List<Map<String, Object>> rows = new ArrayList<>();
        String fileName = file.getOriginalFilename();
        try (InputStream inputStream = file.getInputStream()) {
            EasyExcel.read(inputStream, new AnalysisEventListener<Map<Integer, String>>() {
                // The mapping of column index and header name
                private Map<Integer, String> headerMap = new HashMap<>();

                @Override
                public void invoke(Map<Integer, String> rowData, AnalysisContext context) {
                    // Create a row data Map
                    Map<String, Object> mappedRow = new HashMap<>();
                    for (Map.Entry<Integer, String> entry : rowData.entrySet()) {
                        String headerName = headerMap.get(entry.getKey());
                        String fieldName = headerToFieldMap.get(headerName);
                        if (fieldName != null) {
                            mappedRow.put(fieldName, entry.getValue());
                        }
                    }
                    rows.add(mappedRow);
                }

                // Save the header mapping
                @Override
                public void invokeHeadMap(Map<Integer, String> headMap, AnalysisContext context) {
                    this.headerMap = headMap;
                }

                @Override
                public void doAfterAllAnalysed(AnalysisContext context) {
                    log.info("All data processed!");
                }
            }).sheet(0).doRead();
        } catch (IOException e) {
            throw new BusinessException("Failed to read uploaded Excel file {0}", fileName, e);
        }
        return rows;
    }

    /**
     * Generate an import history record.
     *
     * @param importTemplateId the ID of the import template
     * @param fileId the fileId of the exported file in FileRecord model
     * @return the generated importHistory object
     */
    protected ImportHistory generateExportHistory(String fileName, Long importTemplateId, Long fileId, Long failedFileId) {
        ImportHistory importHistory = new ImportHistory();
        importHistory.setFileName(fileName);
        importHistory.setTemplateId(importTemplateId);
        importHistory.setFileId(fileId);
        importHistory.setFailedFileId(failedFileId);
        importHistory.setStatus(ImportStatus.PROCESSING);
        Long id = importHistoryService.createOne(importHistory);
        importHistory.setId(id);
        return importHistory;
    }

    /**
     * Generate an Excel file consist of failed data.
     *
     * @param fileName the name of the file with failed data
     * @param headers the list of header labels
     * @param fields the list of field names
     * @param failedDataList the list of failed data
     * @return the fileId of the generated Excel file with failed data
     */
    private Long generateFailedExcel(String fileName, List<String> headers, List<String> fields, List<Map<String, Object>> failedDataList) {
        fileName = fileName + "_" + FAILED_LABEL;
        // Get the data to be exported
        List<List<String>> headerList = headers.stream().map(Collections::singletonList).toList();
        // Get the data to be exported
        List<List<Object>> rowsTable = ListUtils.convertToTableData(fields, failedDataList);
        FileInfo fileInfo = commonExport.generateFileAndUpload(fileName, FAILED_LABEL, headerList, rowsTable);
        return fileInfo.getFileId();
    }

}
