package info.openmeta.starter.file.excel;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.write.metadata.WriteSheet;
import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.DateUtils;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.Page;
import info.openmeta.framework.orm.enums.FileType;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.starter.file.entity.ExportHistory;
import info.openmeta.starter.file.entity.ExportTemplate;
import info.openmeta.starter.file.enums.FileSource;
import info.openmeta.starter.file.service.ExportHistoryService;
import info.openmeta.starter.file.service.FileRecordService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Base export.
 */
@Slf4j
@Component
class BaseExport {

    @Autowired
    private ModelService<?> modelService;

    @Autowired
    private FileRecordService fileRecordService;

    @Autowired
    private ExportHistoryService exportHistoryService;

    /**
     * Validate the exported by file template configuration.
     *
     * @param modelName the model name to be exported
     * @param exportTemplate the export template to be validated
     */
    protected void validateFileTemplate(String modelName, ExportTemplate exportTemplate) {
        Assert.notNull(exportTemplate, "The export template does not exist.");
        Assert.isTrue(StringUtils.hasText(exportTemplate.getModelName()),
                "The model name in the export template cannot be empty.");
        Assert.isTrue(exportTemplate.getModelName().equals(modelName),
                "The model name in the export template is inconsistent with the model name to be exported");
    }

    /**
     * Get the data to be exported.
     * Set the convertType to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param modelName the model name
     * @return the data to be exported
     */
    protected List<Map<String, Object>> getExportedData(String modelName, FlexQuery flexQuery) {
        Page<Map<String, Object>> page = Page.ofCursorPage(BaseConstant.MAX_BATCH_SIZE);
        List<Map<String, Object>> exportedRows = new ArrayList<>();
        do {
            page = modelService.searchPage(modelName, flexQuery, page);
            if (!page.getRows().isEmpty()) {
                exportedRows.addAll(page.getRows());
            }
        } while (page.toNext());
        return exportedRows;
    }

    /**
     * Generate the Excel file and upload it to the file storage.
     *
     * @param fileName the name of the file
     * @param headerList the list of header labels
     * @param rowsTable the data table of the rows
     * @return the file info object with download URL
     */
    public FileInfo generateFileAndUpload(String fileName, List<List<String>> headerList, List<List<Object>> rowsTable) {
        fileName = fileName + DateUtils.getCurrentSimpleDateString();
        // Generate the Excel file
        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
             // Use EasyExcel to write the file with dynamic headers and data
             ExcelWriter excelWriter = EasyExcel.write(outputStream).build()) {
            // Write the header and data
            WriteSheet writeSheet = EasyExcel.writerSheet().head(headerList).build();
            excelWriter.write(rowsTable, writeSheet);
            excelWriter.finish();
            // Convert ByteArrayOutputStream to InputStream for return and upload
            InputStream resultStream = new ByteArrayInputStream(outputStream.toByteArray());
            return fileRecordService.uploadFile(fileName, FileType.XLSX, resultStream, FileSource.DOWNLOAD);
        } catch (Exception e) {
            throw new BusinessException("Error generating Excel from template {0} with the provided data.", fileName, e);
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
