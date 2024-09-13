package info.openmeta.starter.file.excel;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.write.metadata.WriteSheet;
import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Orders;
import info.openmeta.framework.orm.enums.FileType;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.utils.ListUtils;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.starter.file.entity.ExportTemplate;
import info.openmeta.starter.file.entity.ExportTemplateField;
import info.openmeta.starter.file.enums.FileSource;
import info.openmeta.starter.file.service.ExportTemplateFieldService;
import info.openmeta.starter.file.service.FileRecordService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Export by template.
 */
@Slf4j
@Component
public class ExportByTemplate extends CommonExport {

    @Autowired
    private FileRecordService fileRecordService;

    @Autowired
    private ExportTemplateFieldService exportTemplateFieldService;

    /**
     * Export data by exportTemplate configured exported fields.
     * The convertType should be set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param flexQuery the flex query to be used for data retrieval
     * @param exportTemplate exportTemplate object
     * @return fileInfo object with download URL
     */
    public FileInfo export(FlexQuery flexQuery, ExportTemplate exportTemplate) {
        // Construct the headers order by sequence of the export fields
        List<String> fieldNames = new ArrayList<>();
        List<List<String>> headerList = new ArrayList<>();
        this.extractFieldsAndLabels(exportTemplate.getModelName(), exportTemplate.getId(), fieldNames, headerList);
        // Get the data to be exported
        flexQuery.setFields(fieldNames);
        List<Map<String, Object>> rows = this.getExportedData(exportTemplate.getModelName(), flexQuery);
        List<List<Object>> rowsTable = ListUtils.convertToTableData(fieldNames, rows);
        // Generate the Excel file
        String fileName = exportTemplate.getFileName();
        String sheetName = StringUtils.hasText(exportTemplate.getSheetName()) ? exportTemplate.getSheetName() : fileName;
        FileInfo fileInfo = this.generateFileAndUpload(fileName, sheetName, headerList, rowsTable);
        // Generate an export history record
        this.generateExportHistory(exportTemplate.getId(), fileInfo.getFileId());
        return fileInfo;
    }

    /**
     * Export multiple sheets of data by dynamic fields and QueryParams, without export template.
     * The convertType should be set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param fileName the name of the Excel file to be exported
     * @param exportTemplates the list of exportTemplates
     * @return fileInfo object with download URL
     */
    public FileInfo exportMultiSheet(String fileName, List<ExportTemplate> exportTemplates) {
        FileInfo fileInfo;
        // Generate the Excel file
        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
             // Use EasyExcel to write the file with dynamic headers and data
             ExcelWriter excelWriter = EasyExcel.write(outputStream).build()) {
            for (int i = 0; i < exportTemplates.size(); i++) {
                ExportTemplate template = exportTemplates.get(i);
                List<String> fieldNames = new ArrayList<>();
                List<List<String>> headerList = new ArrayList<>();
                this.extractFieldsAndLabels(template.getModelName(), template.getId(), fieldNames, headerList);
                // Get the data to be exported
                FlexQuery flexQuery = new FlexQuery(fieldNames, template.getFilters(), template.getOrders());
                flexQuery.setFields(fieldNames);
                List<Map<String, Object>> rows = this.getExportedData(template.getModelName(), flexQuery);
                List<List<Object>> rowsTable = ListUtils.convertToTableData(fieldNames, rows);
                // Write the header and data
                String sheetName = StringUtils.hasText(template.getSheetName()) ? template.getSheetName() : template.getFileName();
                WriteSheet writeSheet = EasyExcel.writerSheet(i, sheetName).head(headerList).build();
                excelWriter.write(rowsTable, writeSheet);
            }
            excelWriter.finish();
            // Convert ByteArrayOutputStream to InputStream for return and upload
            InputStream resultStream = new ByteArrayInputStream(outputStream.toByteArray());
            fileInfo = fileRecordService.uploadFile(fileName, FileType.XLSX, resultStream, FileSource.DOWNLOAD);
        } catch (Exception e) {
            throw new BusinessException("Error generating Excel {0} with the provided data.", fileName, e);
        }
        // Generate an export history record
        this.generateExportHistory(null, fileInfo.getFileId());
        return fileInfo;
    }

    /**
     * Extract the fields and labels from the export template.
     *
     * @param modelName the model name to be exported
     * @param exportTemplateId the ID of the export template
     * @param fieldNames the list of field names
     * @param headerList the list of header labels
     */
    private void extractFieldsAndLabels(String modelName, Long exportTemplateId,
                                        List<String> fieldNames, List<List<String>> headerList) {
        // Construct the headers order by sequence of the export fields
        Filters filters = Filters.eq(ExportTemplateField::getTemplateId, exportTemplateId);
        Orders orders = Orders.ofAsc(ExportTemplateField::getSequence);
        List<ExportTemplateField> exportFields = exportTemplateFieldService.searchList(new FlexQuery(filters, orders));
        exportFields.forEach(exportField -> {
            fieldNames.add(exportField.getFieldName());
            if (StringUtils.hasText(exportField.getCustomHeader())) {
                headerList.add(Collections.singletonList(exportField.getCustomHeader()));
            } else {
                String labelName = ModelManager.getCascadingFieldLabelName(modelName, exportField.getFieldName());
                headerList.add(Collections.singletonList(labelName));
            }
        });
    }
}
