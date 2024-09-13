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
import info.openmeta.starter.file.dto.ImportConfigDTO;
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
        // Construct the headers order by sequence of the export fields
        Map<String, ImportFieldDTO> headerToFieldDTOMap = this.getHeaderToFieldDTOMap(importTemplate);
        List<List<String>> headers = headerToFieldDTOMap.keySet().stream().map(Collections::singletonList).toList();
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
        // generate the ImportDataDTO object and ImportConfigDTO object
        Map<String, ImportFieldDTO> headerToFieldDTOMap = this.getHeaderToFieldDTOMap(importTemplate);
        ImportDataDTO importDataDTO = this.generateImportDataDTO(headerToFieldDTOMap, file);
        ImportConfigDTO importConfigDTO = this.convertToImportConfigDTO(importTemplate);
        // import and return the failed data
        dataHandler.importData(importConfigDTO, importDataDTO);
        String fileName = FileUtils.getShortFileName(file);
        Long fileId = fileRecordService.uploadFile(fileName, file);
        Long failedFileId = null;
        if (!CollectionUtils.isEmpty(importDataDTO.getFailedRows())) {
            failedFileId = generateFailedExcel(fileName, importDataDTO);
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
        List<ImportFieldDTO> importFieldDTOList = importWizard.getImportFieldDTOList();
        Map<String, ImportFieldDTO> headerToFieldDTOMap = new LinkedHashMap<>();
        importFieldDTOList.forEach(importFieldDTO -> headerToFieldDTOMap.put(importFieldDTO.getHeader(), importFieldDTO));
        // generate the ImportDataDTO object and ImportConfigDTO object
        ImportDataDTO importDataDTO = this.generateImportDataDTO(headerToFieldDTOMap, importWizard.getFile());
        ImportConfigDTO importConfigDTO = this.convertToImportConfigDTO(importWizard);
        // import and return the failed data
        dataHandler.importData(importConfigDTO, importDataDTO);
        String fileName = importWizard.getFileName();
        Long fileId = fileRecordService.uploadFile(fileName, importWizard.getFile());
        Long failedFileId = null;
        if (!CollectionUtils.isEmpty(importDataDTO.getFailedRows())) {
            failedFileId = generateFailedExcel(fileName, importDataDTO);
        }
        // Generate an export history record
        return this.generateExportHistory(fileName, null, fileId, failedFileId);
    }

    /**
     * Generate the ImportDataDTO object to store the data during the import process.
     *
     * @param headerToFieldDTOMap the mapping of the header to ImportFieldDTO
     * @param file           the uploaded file to be imported
     * @return the generated ImportDataDTO object
     */
    private ImportDataDTO generateImportDataDTO(Map<String, ImportFieldDTO> headerToFieldDTOMap, MultipartFile file) {
        List<String> headers = new ArrayList<>();
        List<String> fields = new ArrayList<>();
        Set<String> requiredFields = new HashSet<>();
        headerToFieldDTOMap.forEach((header, importField) -> {
            headers.add(header);
            fields.add(importField.getFieldName());
            if (Boolean.TRUE.equals(importField.getRequired())) {
                requiredFields.add(importField.getFieldName());
            }
        });
        List<Map<String, Object>> allDataList = this.extractDataFromExcel(headerToFieldDTOMap, file);
        ImportDataDTO importDataDTO = new ImportDataDTO(fields, requiredFields, allDataList);
        importDataDTO.setHeaders(headers);
        return importDataDTO;
    }

    /**
     * Get the mapping of the header to the import field.
     *
     * @param importTemplate the import template object
     */
    public Map<String, ImportFieldDTO> getHeaderToFieldDTOMap(ImportTemplate importTemplate) {
        Map<String, ImportFieldDTO> headerToFieldMap = new LinkedHashMap<>();
        // Construct the headers order by sequence of the export fields
        Filters filters = Filters.eq(ImportTemplateField::getTemplateId, importTemplate.getId());
        Orders orders = Orders.ofAsc(ImportTemplateField::getSequence);
        List<ImportTemplateField> importTemplateFields = importTemplateFieldService.searchList(new FlexQuery(filters, orders));
        importTemplateFields.forEach(importTemplateField -> {
            ImportFieldDTO importFieldDTO = convertToImportFieldDTO(importTemplate.getModelName(), importTemplateField);
            headerToFieldMap.put(importFieldDTO.getHeader(), importFieldDTO);
        });
        return headerToFieldMap;
    }

    /**
     * Convert the importTemplate to the importConfigDTO.
     *
     * @param importTemplate the importTemplate object
     * @return the converted importConfigDTO
     */
    private ImportConfigDTO convertToImportConfigDTO(ImportTemplate importTemplate) {
        ImportConfigDTO importConfigDTO = new ImportConfigDTO();
        importConfigDTO.setModelName(importTemplate.getModelName());
        importConfigDTO.setImportRule(importTemplate.getImportRule());
        importConfigDTO.setIgnoreEmpty(importTemplate.getIgnoreEmpty());
        importConfigDTO.setSkipException(importTemplate.getSkipException());
        importConfigDTO.setUniqueConstraints(importTemplate.getUniqueConstraints());
        return importConfigDTO;
    }

    /**
     * Convert the importWizard to the importConfigDTO.
     *
     * @param importWizard the importWizard object
     * @return the converted importConfigDTO
     */
    private ImportConfigDTO convertToImportConfigDTO(ImportWizard importWizard) {
        ImportConfigDTO importConfigDTO = new ImportConfigDTO();
        importConfigDTO.setModelName(importWizard.getModelName());
        importConfigDTO.setImportRule(importWizard.getImportRule());
        importConfigDTO.setIgnoreEmpty(importWizard.getIgnoreEmpty());
        importConfigDTO.setSkipException(importWizard.getSkipException());
        List<String> uniqueConstraints = StringUtils.hasText(importWizard.getUniqueConstraints()) ?
                List.of(importWizard.getUniqueConstraints().split(",")) : Collections.emptyList();
        importConfigDTO.setUniqueConstraints(uniqueConstraints);
        return importConfigDTO;
    }

    /**
     * Convert the import template field to the import field DTO.
     *
     * @param modelName          the name of the model
     * @param importTemplateField the import template field
     * @return the converted import field DTO
     */
    private ImportFieldDTO convertToImportFieldDTO(String modelName, ImportTemplateField importTemplateField) {
        ImportFieldDTO importFieldDTO = new ImportFieldDTO();
        importFieldDTO.setFieldName(importTemplateField.getFieldName());
        importFieldDTO.setRequired(importTemplateField.getRequired());
        if (StringUtils.hasText(importTemplateField.getCustomHeader())) {
            importFieldDTO.setHeader(importTemplateField.getCustomHeader());
        } else {
            String labelName = ModelManager.getCascadingFieldLabelName(modelName, importTemplateField.getFieldName());
            importFieldDTO.setHeader(labelName);
        }
        return importFieldDTO;
    }

    /**
     * Extract the fields and labels from the export template.
     *
     * @param headerToFieldDTOMap the mapping of the header to ImportFieldDTO
     * @param file the uploaded file to be imported
     */
    private List<Map<String, Object>> extractDataFromExcel(Map<String, ImportFieldDTO> headerToFieldDTOMap, MultipartFile file) {
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
                        String fieldName = headerToFieldDTOMap.get(headerName).getFieldName();
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
     * @param importDataDTO the import data DTO
     * @return the fileId of the generated Excel file with failed data
     */
    private Long generateFailedExcel(String fileName, ImportDataDTO importDataDTO) {
        fileName = fileName + "_" + FAILED_LABEL;
        // Get the data to be exported
        List<List<String>> headerList = importDataDTO.getHeaders().stream().map(Collections::singletonList).toList();
        // Get the data to be exported
        List<List<Object>> rowsTable = ListUtils.convertToTableData(importDataDTO.getFields(), importDataDTO.getFailedRows());
        FileInfo fileInfo = commonExport.generateFileAndUpload(fileName, FAILED_LABEL, headerList, rowsTable);
        return fileInfo.getFileId();
    }

}
