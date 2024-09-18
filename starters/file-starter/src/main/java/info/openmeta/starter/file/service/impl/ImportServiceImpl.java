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
import info.openmeta.starter.file.dto.ImportTemplateDTO;
import info.openmeta.starter.file.dto.ImportDataDTO;
import info.openmeta.starter.file.dto.ImportFieldDTO;
import info.openmeta.starter.file.entity.ImportHistory;
import info.openmeta.starter.file.entity.ImportTemplate;
import info.openmeta.starter.file.entity.ImportTemplateField;
import info.openmeta.starter.file.enums.ImportStatus;
import info.openmeta.starter.file.excel.CommonExport;
import info.openmeta.starter.file.excel.ImportHandlerManager;
import info.openmeta.starter.file.service.*;
import info.openmeta.starter.file.vo.ImportWizard;
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
        Assert.notNull(importTemplate.getImportRule(),
                "Import template `{0}` importRule cannot be null.", importTemplate.getName());
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
        // Construct the importTemplateDTO object
        ImportTemplateDTO importTemplateDTO = this.getImportTemplateDTO(importTemplate);
        List<List<String>> headers = importTemplateDTO.getImportFields().stream()
                .map(ImportFieldDTO::getHeader)
                .map(Collections::singletonList).toList();
        // Generate the Excel file
        String fileName = importTemplate.getName();
        String sheetName = importTemplate.getName();
        return commonExport.generateFileAndUpload(fileName, sheetName, headers, Collections.emptyList());
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
        // generate the ImportDataDTO object and ImportTemplateDTO object
        ImportTemplateDTO importTemplateDTO = this.getImportTemplateDTO(importTemplate);
        ImportDataDTO importDataDTO = this.generateImportDataDTO(importTemplateDTO, file);
        // import and return the failed data
        dataHandler.importData(importTemplateDTO, importDataDTO);
        String fileName = FileUtils.getShortFileName(file);
        Long fileId = fileRecordService.uploadFile(fileName, file);
        Long failedFileId = null;
        if (!CollectionUtils.isEmpty(importDataDTO.getFailedRows())) {
            failedFileId = generateFailedExcel(fileName, importTemplateDTO, importDataDTO);
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
    public ImportHistory importByDynamic(ImportWizard importWizard) {
        ImportTemplateDTO importTemplateDTO = this.convertToImportTemplateDTO(importWizard);
        // generate the ImportDataDTO object and ImportTemplateDTO object
        ImportDataDTO importDataDTO = this.generateImportDataDTO(importTemplateDTO, importWizard.getFile());
        // import and return the failed data
        dataHandler.importData(importTemplateDTO, importDataDTO);
        String fileName = importWizard.getFileName();
        Long fileId = fileRecordService.uploadFile(fileName, importWizard.getFile());
        Long failedFileId = null;
        if (!CollectionUtils.isEmpty(importDataDTO.getFailedRows())) {
            failedFileId = generateFailedExcel(fileName, importTemplateDTO, importDataDTO);
        }
        // Generate an export history record
        return this.generateExportHistory(fileName, null, fileId, failedFileId);
    }

    /**
     * Generate the ImportDataDTO object to store the data during the import process.
     *
     * @param importTemplateDTO the importTemplateDTO object
     * @param file           the uploaded file to be imported
     * @return the generated ImportDataDTO object
     */
    private ImportDataDTO generateImportDataDTO(ImportTemplateDTO importTemplateDTO, MultipartFile file) {
        Map<String, String> headerToFieldMap = new HashMap<>();
        importTemplateDTO.getImportFields()
                .forEach(importField -> headerToFieldMap.put(importField.getHeader(), importField.getFieldName()));
        List<Map<String, Object>> allDataList = this.extractDataFromExcel(headerToFieldMap, file);
        ImportDataDTO importDataDTO = new ImportDataDTO();
        importDataDTO.setRows(allDataList);
        return importDataDTO;
    }

    /**
     * Get the ImportTemplateDTO object by importTemplate
     *
     * @param importTemplate the import template object
     * @return importTemplateDTO object
     */
    public ImportTemplateDTO getImportTemplateDTO(ImportTemplate importTemplate) {
        ImportTemplateDTO importTemplateDTO = this.convertToImportTemplateDTO(importTemplate);
        // Construct the headers order by sequence of the export fields
        Filters filters = Filters.eq(ImportTemplateField::getTemplateId, importTemplate.getId());
        Orders orders = Orders.ofAsc(ImportTemplateField::getSequence);
        List<ImportTemplateField> importTemplateFields = importTemplateFieldService.searchList(new FlexQuery(filters, orders));
        importTemplateFields.forEach(importTemplateField -> {
            ImportFieldDTO importFieldDTO = convertToImportFieldDTO(importTemplate, importTemplateField);
            importTemplateDTO.addImportField(importFieldDTO);
        });
        return importTemplateDTO;
    }

    /**
     * Convert the importTemplate to the importTemplateDTO.
     *
     * @param importTemplate the importTemplate object
     * @return the converted importTemplateDTO
     */
    private ImportTemplateDTO convertToImportTemplateDTO(ImportTemplate importTemplate) {
        ImportTemplateDTO importTemplateDTO = new ImportTemplateDTO();
        importTemplateDTO.setModelName(importTemplate.getModelName());
        importTemplateDTO.setImportRule(importTemplate.getImportRule());
        importTemplateDTO.setIgnoreEmpty(importTemplate.getIgnoreEmpty());
        importTemplateDTO.setSkipException(importTemplate.getSkipException());
        importTemplateDTO.setCustomHandler(importTemplate.getCustomHandler());
        importTemplateDTO.setUniqueConstraints(importTemplate.getUniqueConstraints());
        return importTemplateDTO;
    }

    /**
     * Convert the importWizard to the importTemplateDTO.
     *
     * @param importWizard the importWizard object
     * @return the converted importTemplateDTO
     */
    private ImportTemplateDTO convertToImportTemplateDTO(ImportWizard importWizard) {
        ImportTemplateDTO importTemplateDTO = new ImportTemplateDTO();
        importTemplateDTO.setModelName(importWizard.getModelName());
        importTemplateDTO.setImportRule(importWizard.getImportRule());
        importTemplateDTO.setIgnoreEmpty(importWizard.getIgnoreEmpty());
        importTemplateDTO.setSkipException(importWizard.getSkipException());
        importTemplateDTO.setCustomHandler(importWizard.getCustomHandler());
        List<String> uniqueConstraints = StringUtils.hasText(importWizard.getUniqueConstraints()) ?
                List.of(importWizard.getUniqueConstraints().split(",")) : Collections.emptyList();
        importTemplateDTO.setUniqueConstraints(uniqueConstraints);
        // set the import fields
        importTemplateDTO.setImportFields(importWizard.getImportFieldDTOList());
        // update the `ignoreEmpty` of the import fields
        importTemplateDTO.getImportFields()
                .forEach(importFieldDTO -> importFieldDTO.setIgnoreEmpty(importTemplateDTO.getIgnoreEmpty()));
        return importTemplateDTO;
    }

    /**
     * Convert the import template field to the import field DTO.
     *
     * @param importTemplate      the import template
     * @param importTemplateField the import template field
     * @return the converted import field DTO
     */
    private ImportFieldDTO convertToImportFieldDTO(ImportTemplate importTemplate, ImportTemplateField importTemplateField) {
        ImportFieldDTO importFieldDTO = new ImportFieldDTO();
        importFieldDTO.setFieldName(importTemplateField.getFieldName());
        importFieldDTO.setRequired(importTemplateField.getRequired());
        importFieldDTO.setDefaultValue(importTemplateField.getDefaultValue());
        importFieldDTO.setIgnoreEmpty(importTemplate.getIgnoreEmpty());
        if (StringUtils.hasText(importTemplateField.getCustomHeader())) {
            importFieldDTO.setHeader(importTemplateField.getCustomHeader());
        } else {
            String labelName = ModelManager.getCascadingFieldLabelName(importTemplate.getModelName(), importTemplateField.getFieldName());
            importFieldDTO.setHeader(labelName);
        }
        return importFieldDTO;
    }

    /**
     * Extract the fields and labels from the export template.
     *
     * @param headerToFieldMap the mapping of the header to fieldName
     * @param file the uploaded file to be imported
     */
    private List<Map<String, Object>> extractDataFromExcel(Map<String, String> headerToFieldMap, MultipartFile file) {
        List<Map<String, Object>> rows = new ArrayList<>();
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
            String fileName = file.getOriginalFilename();
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
     * @param importTemplateDTO the import template DTO
     * @param importDataDTO the import data DTO
     * @return the fileId of the generated Excel file with failed data
     */
    private Long generateFailedExcel(String fileName, ImportTemplateDTO importTemplateDTO, ImportDataDTO importDataDTO) {
        fileName = fileName + "_" + FAILED_LABEL;
        List<List<String>> headers = new ArrayList<>();
        List<String> fields = new ArrayList<>();
        importTemplateDTO.getImportFields().forEach(importFieldDTO -> {
            fields.add(importFieldDTO.getHeader());
            headers.add(Collections.singletonList(importFieldDTO.getHeader()));
        });
        // Get the data to be exported
        List<List<Object>> rowsTable = ListUtils.convertToTableData(fields, importDataDTO.getFailedRows());
        FileInfo fileInfo = commonExport.generateFileAndUpload(fileName, FAILED_LABEL, headers, rowsTable);
        return fileInfo.getFileId();
    }

}
