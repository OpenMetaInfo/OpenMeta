package info.openmeta.starter.file.service.impl;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.event.AnalysisEventListener;
import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.base.i18n.I18n;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.utils.ListUtils;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.framework.web.utils.FileUtils;
import info.openmeta.starter.file.constant.FileConstant;
import info.openmeta.starter.file.dto.ImportDataDTO;
import info.openmeta.starter.file.dto.ImportFieldDTO;
import info.openmeta.starter.file.dto.ImportTemplateDTO;
import info.openmeta.starter.file.entity.ImportHistory;
import info.openmeta.starter.file.entity.ImportTemplate;
import info.openmeta.starter.file.entity.ImportTemplateField;
import info.openmeta.starter.file.enums.ImportStatus;
import info.openmeta.starter.file.excel.CommonExport;
import info.openmeta.starter.file.excel.ImportHandlerManager;
import info.openmeta.starter.file.message.AsyncImportProducer;
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

    @Autowired
    private AsyncImportProducer asyncImportProducer;

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
        ImportTemplateDTO importTemplateDTO = this.getImportTemplateDTO(importTemplate, null);
        List<String> headers = importTemplateDTO.getImportFields().stream()
                .map(ImportFieldDTO::getHeader).toList();
        // Generate the Excel file
        String fileName = importTemplate.getName();
        String sheetName = importTemplate.getName();
        return commonExport.generateFileAndUpload(fileName, sheetName, headers, Collections.emptyList());
    }

    /**
     * Import data from the uploaded file and the import template ID
     *
     * @param templateId the ID of the import template
     * @param file the uploaded file
     * @param env the environment variables
     * @return the import result
     */
    @Override
    public ImportHistory importByTemplate(Long templateId, MultipartFile file, Map<String, Object> env) {
        ImportTemplate importTemplate = importTemplateService.readOne(templateId);
        this.validateImportTemplate(importTemplate);
        String fileName = FileUtils.getShortFileName(file);
        Long fileId = fileRecordService.uploadFile(fileName, file);
        // Generate an export history record
        ImportHistory importHistory = this.generateImportHistory(fileName, templateId, fileId);
        // generate the ImportDataDTO object and ImportTemplateDTO object
        ImportTemplateDTO importTemplateDTO = this.getImportTemplateDTO(importTemplate, env);
        importTemplateDTO.setFileId(fileId);
        importTemplateDTO.setHistoryId(importHistory.getId());
        importTemplateDTO.setFileName(fileName);
        if (Boolean.TRUE.equals(importTemplate.getSyncImport())) {
            try (InputStream inputStream = file.getInputStream()) {
                return this.syncImport(importTemplateDTO, inputStream, importHistory);
            } catch (IOException e) {
                throw new BusinessException("Failed to read uploaded Excel file {0}", fileName, e);
            }
        } else {
            asyncImportProducer.sendAsyncImport(importTemplateDTO);
        }
        return importHistory;
    }

    /**
     * Synchronous import data from the uploaded file and import template
     *
     * @param importTemplateDTO the import template DTO
     * @param inputStream the input stream of the uploaded file
     * @param importHistory the import history object
     * @return the import history object
     */
    public ImportHistory syncImport(ImportTemplateDTO importTemplateDTO, InputStream inputStream, ImportHistory importHistory) {
        ImportDataDTO importDataDTO = this.generateImportDataDTO(importTemplateDTO, inputStream);
        dataHandler.importData(importTemplateDTO, importDataDTO);
        if (!CollectionUtils.isEmpty(importDataDTO.getFailedRows())) {
            Long failedFileId = this.generateFailedExcel(importTemplateDTO.getFileName(), importTemplateDTO, importDataDTO);
            importHistory.setFailedFileId(failedFileId);
            ImportStatus status = CollectionUtils.isEmpty(importDataDTO.getRows()) ?
                    ImportStatus.FAILURE : ImportStatus.PARTIAL_FAILURE;
            importHistory.setStatus(status);
        } else {
            importHistory.setStatus(ImportStatus.SUCCESS);
        }
        importHistoryService.updateOne(importHistory);
        return importHistory;
    }

    /**
     * Import data from the uploaded file and dynamic import settings
     *
     * @return the import result
     */
    @Override
    public ImportHistory importByDynamic(ImportWizard importWizard) {
        String fileName = importWizard.getFileName();
        Long fileId = fileRecordService.uploadFile(fileName, importWizard.getFile());
        // Generate an export history record
        ImportHistory importHistory = this.generateImportHistory(fileName, null, fileId);
        // generate the ImportDataDTO object and ImportTemplateDTO object
        ImportTemplateDTO importTemplateDTO = this.convertToImportTemplateDTO(importWizard);
        importTemplateDTO.setFileId(fileId);
        importTemplateDTO.setHistoryId(importHistory.getId());
        importTemplateDTO.setFileName(fileName);
        if (Boolean.TRUE.equals(importWizard.getSyncImport())) {
            try (InputStream inputStream = importWizard.getFile().getInputStream()) {
                return this.syncImport(importTemplateDTO, inputStream, importHistory);
            } catch (IOException e) {
                throw new BusinessException("Failed to read uploaded Excel file {0}", fileName, e);
            }
        } else {
            asyncImportProducer.sendAsyncImport(importTemplateDTO);
        }
        return importHistory;
    }

    /**
     * Generate the ImportDataDTO object to store the data during the import process.
     *
     * @param importTemplateDTO the importTemplateDTO object
     * @param inputStream the input stream of the uploaded file
     * @return the generated ImportDataDTO object
     */
    private ImportDataDTO generateImportDataDTO(ImportTemplateDTO importTemplateDTO, InputStream inputStream) {
        Map<String, String> headerToFieldMap = new HashMap<>();
        importTemplateDTO.getImportFields()
                .forEach(importField -> headerToFieldMap.put(importField.getHeader(), importField.getFieldName()));
        List<Map<String, Object>> allDataList = this.extractDataFromExcel(headerToFieldMap, inputStream);
        Assert.notEmpty(allDataList, "No data exists in the excel file `{0}`", importTemplateDTO.getFileName());
        ImportDataDTO importDataDTO = new ImportDataDTO();
        importDataDTO.setRows(allDataList);
        importDataDTO.setEnv(importTemplateDTO.getEnv());
        importDataDTO.setOriginalRows(ListUtils.deepCopy(allDataList));
        return importDataDTO;
    }

    /**
     * Get the ImportTemplateDTO object by importTemplate
     *
     * @param importTemplate the import template object
     * @return importTemplateDTO object
     */
    public ImportTemplateDTO getImportTemplateDTO(ImportTemplate importTemplate, Map<String, Object> env) {
        ImportTemplateDTO importTemplateDTO = this.convertToImportTemplateDTO(importTemplate, env);
        // Construct the headers order by sequence of the export fields
        Filters filters = Filters.eq(ImportTemplateField::getTemplateId, importTemplate.getId());
        Orders orders = Orders.ofAsc(ImportTemplateField::getSequence);
        List<ImportTemplateField> importTemplateFields = importTemplateFieldService.searchList(new FlexQuery(filters, orders));
        importTemplateFields.forEach(importTemplateField -> {
            ImportFieldDTO importFieldDTO = convertToImportFieldDTO(importTemplateDTO, importTemplateField);
            importTemplateDTO.addImportField(importFieldDTO);
        });
        return importTemplateDTO;
    }

    /**
     * Convert the importTemplate to the importTemplateDTO.
     *
     * @param importTemplate the importTemplate object
     * @param env the environment variables
     * @return the converted importTemplateDTO
     */
    private ImportTemplateDTO convertToImportTemplateDTO(ImportTemplate importTemplate, Map<String, Object> env) {
        ImportTemplateDTO importTemplateDTO = new ImportTemplateDTO();
        importTemplateDTO.setTemplateId(importTemplate.getId());
        importTemplateDTO.setModelName(importTemplate.getModelName());
        importTemplateDTO.setImportRule(importTemplate.getImportRule());
        importTemplateDTO.setIgnoreEmpty(importTemplate.getIgnoreEmpty());
        importTemplateDTO.setSkipException(importTemplate.getSkipException());
        importTemplateDTO.setCustomHandler(importTemplate.getCustomHandler());
        importTemplateDTO.setUniqueConstraints(importTemplate.getUniqueConstraints());
        importTemplateDTO.setEnv(env);
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
     * @param importTemplateDTO      the importTemplateDTO object
     * @param importTemplateField the import template field
     * @return the converted import field DTO
     */
    private ImportFieldDTO convertToImportFieldDTO(ImportTemplateDTO importTemplateDTO, ImportTemplateField importTemplateField) {
        ImportFieldDTO importFieldDTO = new ImportFieldDTO();
        importFieldDTO.setFieldName(importTemplateField.getFieldName());
        importFieldDTO.setRequired(importTemplateField.getRequired());
        importFieldDTO.setIgnoreEmpty(importTemplateDTO.getIgnoreEmpty());
        // Get the metaField object of the last field in cascading `fieldName`.
        MetaField lastField = ModelManager.getLastFieldOfCascaded(importTemplateDTO.getModelName(), importTemplateField.getFieldName());
        // Set the default value of the imported field
        if (StringUtils.hasText(importTemplateField.getDefaultValue())) {
            Object defaultValue = this.getDefaultValue(lastField.getFieldType(), importTemplateField.getDefaultValue(), importTemplateDTO.getEnv());
            importFieldDTO.setDefaultValue(defaultValue);
        }
        // If the custom header is not set, use the labelName of the field as the header
        if (StringUtils.hasText(importTemplateField.getCustomHeader())) {
            importFieldDTO.setHeader(importTemplateField.getCustomHeader());
        } else {
            importFieldDTO.setHeader(lastField.getLabelName());
        }
        return importFieldDTO;
    }

    /**
     * Get the default value of the field.
     *
     * @param fieldType the field type
     * @param defaultValue the default value
     * @param env the environment variables
     * @return the default value
     */
    private Object getDefaultValue(FieldType fieldType, String defaultValue, Map<String, Object> env) {
        if (StringTools.isVariable(defaultValue)) {
            return StringTools.extractVariable(defaultValue, env);
        } else {
            return FieldType.convertStringToObject(fieldType, defaultValue);
        }
    }

    /**
     * Extract the data from the uploaded Excel file.
     * Load the first sheet of the Excel file by EasyExcel.read(file, {}).sheet(0).doRead().
     *
     * @param headerToFieldMap the mapping of the header to fieldName
     * @param inputStream the input stream of the uploaded file
     */
    private List<Map<String, Object>> extractDataFromExcel(Map<String, String> headerToFieldMap, InputStream inputStream) {
        List<Map<String, Object>> rows = new ArrayList<>();
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
        return rows;
    }

    /**
     * Generate an import history record.
     *
     * @param importTemplateId the ID of the import template
     * @param fileId the fileId of the exported file in FileRecord model
     * @return the generated importHistory object
     */
    protected ImportHistory generateImportHistory(String fileName, Long importTemplateId, Long fileId) {
        ImportHistory importHistory = new ImportHistory();
        importHistory.setFileName(fileName);
        importHistory.setTemplateId(importTemplateId);
        importHistory.setFileId(fileId);
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
        fileName = fileName + "_" + FileConstant.FAILED_DATA;
        List<String> headers = new ArrayList<>();
        List<String> fields = new ArrayList<>();
        importTemplateDTO.getImportFields().forEach(importFieldDTO -> {
            fields.add(importFieldDTO.getFieldName());
            headers.add(importFieldDTO.getHeader());
        });
        // Add the failed reason header and column
        fields.add(FileConstant.FAILED_REASON);
        headers.add(I18n.get(FileConstant.FAILED_REASON));
        // Get the data to be exported
        List<List<Object>> rowsTable = ListUtils.convertToTableData(fields, importDataDTO.getFailedRows());
        FileInfo fileInfo = commonExport.generateFileAndUpload(fileName, FileConstant.FAILED_DATA, headers, rowsTable);
        return fileInfo.getFileId();
    }

}
