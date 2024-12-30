package info.openmeta.starter.file.excel;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.write.builder.ExcelWriterSheetBuilder;
import com.alibaba.excel.write.handler.CellWriteHandler;
import com.alibaba.excel.write.handler.SheetWriteHandler;
import com.alibaba.excel.write.handler.WriteHandler;
import com.alibaba.excel.write.metadata.WriteSheet;
import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.SpringContextUtils;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.orm.utils.ListUtils;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.starter.file.dto.ExcelDataDTO;
import info.openmeta.starter.file.entity.ExportHistory;
import info.openmeta.starter.file.entity.ExportTemplate;
import info.openmeta.starter.file.entity.ExportTemplateField;
import info.openmeta.starter.file.excel.handler.CustomExportHandler;
import info.openmeta.starter.file.service.ExportHistoryService;
import info.openmeta.starter.file.service.ExportTemplateFieldService;
import info.openmeta.starter.file.service.FileRecordService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Base export.
 */
@Slf4j
@Component
public class CommonExport {

    @Autowired
    private ModelService<?> modelService;

    @Autowired
    private FileRecordService fileRecordService;

    @Autowired
    private ExportHistoryService exportHistoryService;

    @Autowired
    private ExportTemplateFieldService exportTemplateFieldService;

    /**
     * Fill in the headers by the export template.
     *
     * @param exportTemplate the export template
     * @param excelDataDTO the Excel data DTO
     */
    protected void fillHeadersByTemplate(ExportTemplate exportTemplate, ExcelDataDTO excelDataDTO) {
        List<String> headers = new ArrayList<>();
        List<String> fieldNames = new ArrayList<>();
        List<String> ignoreFields = new ArrayList<>();
        Filters filters = new Filters().eq(ExportTemplateField::getTemplateId, exportTemplate.getId());
        Orders orders = Orders.ofAsc(ExportTemplateField::getSequence);
        List<ExportTemplateField> exportFields = exportTemplateFieldService.searchList(new FlexQuery(filters, orders));
        exportFields.forEach(exportField -> {
            fieldNames.add(exportField.getFieldName());
            // If the field is ignored, add it to the ignore list, otherwise add to the headers list
            if (Boolean.TRUE.equals(exportField.getIgnore())) {
                ignoreFields.add(exportField.getFieldName());
            } else if (StringUtils.hasText(exportField.getCustomHeader())) {
                headers.add(exportField.getCustomHeader());
            } else {
                MetaField lastField = ModelManager.getLastFieldOfCascaded(exportTemplate.getModelName(), exportField.getFieldName());
                headers.add(lastField.getLabelName());
            }

        });
        excelDataDTO.setHeaders(headers);
        excelDataDTO.setFetchFields(fieldNames);
        excelDataDTO.setIgnoreFields(ignoreFields);
    }

    /**
     * Fill in the rows by the export template.
     *
     * @param exportTemplate the export template
     * @param flexQuery the flex query
     * @param excelDataDTO the Excel data DTO
     */
    protected void fillRowsByTemplate(ExportTemplate exportTemplate, FlexQuery flexQuery, ExcelDataDTO excelDataDTO) {
        // Get the data to be exported
        List<String> fieldNames = excelDataDTO.getFetchFields();
        flexQuery.setFields(fieldNames);
        List<Map<String, Object>> rows = this.getExportedRows(exportTemplate.getModelName(),
                exportTemplate.getCustomHandler(), flexQuery);
        // Remove the ignored fields when exporting
        List<String> exportFields = new ArrayList<>(fieldNames);
        exportFields.removeAll(excelDataDTO.getIgnoreFields());
        List<List<Object>> rowsTable = ListUtils.convertToTableData(exportFields, rows);
        excelDataDTO.setRowsTable(rowsTable);
    }

    /**
     * Get the data to be exported.
     * Set the convertType to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param modelName the model name
     * @return the data to be exported
     */
    protected List<Map<String, Object>> getExportedRows(String modelName, String handlerName, FlexQuery flexQuery) {
        Page<Map<String, Object>> page = Page.ofCursorPage(BaseConstant.MAX_BATCH_SIZE);
        List<Map<String, Object>> exportedRows = new ArrayList<>();
        do {
            page = modelService.searchPage(modelName, flexQuery, page);
            if (!page.getRows().isEmpty()) {
                exportedRows.addAll(page.getRows());
            }
        } while (page.toNext());
        executeCustomHandler(handlerName, exportedRows);
        return exportedRows;
    }

    /**
     * Execute the custom export handler.
     * @param handlerName the name of the custom export handler
     * @param rows the data to be exported
     */
    private void executeCustomHandler(String handlerName, List<Map<String, Object>> rows) {
        if (StringUtils.hasText(handlerName)) {
            if (!StringTools.isBeanName(handlerName)) {
                throw new IllegalArgumentException("The name of custom export handler `{0}` is invalid.", handlerName);
            }
            try {
                CustomExportHandler handler = SpringContextUtils.getBean(handlerName, CustomExportHandler.class);
                handler.handleExportData(rows);
            } catch (NoSuchBeanDefinitionException e) {
                throw new IllegalArgumentException("The custom export handler `{0}` is not found.", handlerName);
            }
        }
    }

    /**
     * Generate the Excel file and upload it to the file storage.
     *
     * @param modelName the model name
     * @param excelDataDTO the Excel data DTO
     * @return the file info object with download URL
     */
    public FileInfo generateFileAndUpload(String modelName, ExcelDataDTO excelDataDTO) {
        return this.generateFileAndUpload(modelName, excelDataDTO, null);
    }

    /**
     * Generate the Excel file and upload it to the file storage.
     *
     * @param modelName the model name
     * @param excelDataDTO the Excel data DTO
     * @param handler the cell handler
     * @return the file info object with download URL
     */
    public FileInfo generateFileAndUpload(String modelName, ExcelDataDTO excelDataDTO,
                                          WriteHandler handler) {
        // Generate the Excel file
        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
             // Use EasyExcel to write the file with dynamic headers and data
             ExcelWriter excelWriter = EasyExcel.write(outputStream).build()) {
            // Write the header and data, EasyExcel requires the header to be a list of lists
            List<List<String>> headersList = excelDataDTO.getHeaders().stream().map(Collections::singletonList).toList();
            ExcelWriterSheetBuilder builder = EasyExcel.writerSheet(excelDataDTO.getSheetName()).head(headersList);

            // Add custom cells and sheet handler
            builder = handler == null ? builder : builder.registerWriteHandler(handler);

            excelWriter.write(excelDataDTO.getRowsTable(), builder.build());
            excelWriter.finish();
            // Upload the Excel bytes to the file storage
            byte[] excelBytes = outputStream.toByteArray();
            return fileRecordService.uploadExcelBytesToDownload(modelName, excelDataDTO.getFileName(), excelBytes);
        } catch (Exception e) {
            throw new BusinessException("Error generating Excel from template {0} with the provided data.",
                    excelDataDTO.getFileName(), e);
        }
    }

    /**
     * Generate an export history record.
     *
     * @param exportTemplateId the ID of the export template
     * @param fileId the fileId of the exported file in FileRecord model
     */
    protected void generateExportHistory(Long exportTemplateId, Long fileId) {
        ExportHistory exportHistory = new ExportHistory();
        exportHistory.setTemplateId(exportTemplateId);
        exportHistory.setFileId(fileId);
        exportHistoryService.createOne(exportHistory);
    }
}
