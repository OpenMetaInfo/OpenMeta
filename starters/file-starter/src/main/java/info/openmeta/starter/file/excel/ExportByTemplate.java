package info.openmeta.starter.file.excel;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.write.metadata.WriteSheet;
import info.openmeta.framework.base.constant.StringConstant;
import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.starter.file.dto.ExcelDataDTO;
import info.openmeta.starter.file.entity.ExportTemplate;
import info.openmeta.starter.file.service.FileRecordService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.io.ByteArrayOutputStream;
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

    /**
     * Export data by exportTemplate configured exported fields.
     * The convertType should be set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param exportTemplate exportTemplate object
     * @param flexQuery the flex query to be used for data retrieval
     * @return fileInfo object with download URL
     */
    public FileInfo export(ExportTemplate exportTemplate, FlexQuery flexQuery) {
        String fileName = exportTemplate.getFileName();
        String sheetName = exportTemplate.getSheetName();
        // Excel data DTO
        ExcelDataDTO excelDataDTO = new ExcelDataDTO();
        excelDataDTO.setFileName(fileName);
        excelDataDTO.setSheetName(StringUtils.hasText(sheetName) ? sheetName : fileName);
        // Fill in the headers and rows
        this.fillHeadersByTemplate(exportTemplate, excelDataDTO);
        this.fillRowsByTemplate(exportTemplate, flexQuery, excelDataDTO);
        // Generate the Excel file
        FileInfo fileInfo = this.generateFileAndUpload(exportTemplate.getModelName(), excelDataDTO);
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
        return this.getFileInfo(fileName, exportTemplates, Collections.emptyMap());
    }

    public FileInfo dynamicExportMultiSheet(String fileName, List<ExportTemplate> exportTemplates, Map<Long, Filters> dynamicTemplateMap) {
        return this.getFileInfo(fileName, exportTemplates, dynamicTemplateMap);
    }

    private FileInfo getFileInfo(String fileName, List<ExportTemplate> exportTemplates, Map<Long, Filters> dynamicTemplateMap) {
        FileInfo fileInfo;
        // Generate the Excel file
        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
             // Use EasyExcel to write the file with dynamic headers and data
             ExcelWriter excelWriter = EasyExcel.write(outputStream).build()) {
            for (int i = 0; i < exportTemplates.size(); i++) {
                ExportTemplate exportTemplate = exportTemplates.get(i);
                String sheetName = exportTemplate.getSheetName();
                // Excel data DTO
                ExcelDataDTO excelDataDTO = new ExcelDataDTO();
                excelDataDTO.setFileName(fileName);
                excelDataDTO.setSheetName(StringUtils.hasText(sheetName) ? sheetName : fileName);
                // Fill in the headers and rows
                this.fillHeadersByTemplate(exportTemplate, excelDataDTO);

                // Get the data to be exported
                FlexQuery flexQuery = new FlexQuery(exportTemplate.getFilters(), exportTemplate.getOrders());
                flexQuery.setConvertType(ConvertType.DISPLAY);
                flexQuery.setFilters(Filters.and(flexQuery.getFilters(), dynamicTemplateMap.get(exportTemplate.getId())));
                this.fillRowsByTemplate(exportTemplate, flexQuery, excelDataDTO);

                // Write the header and data
                List<List<String>> headerList = excelDataDTO.getHeaders().stream().map(Collections::singletonList).toList();
                WriteSheet writeSheet = EasyExcel.writerSheet(i, sheetName).head(headerList).build();
                excelWriter.write(excelDataDTO.getRowsTable(), writeSheet);
            }
            excelWriter.finish();
            // Upload the Excel stream to OSS
            byte[] excelBytes = outputStream.toByteArray();
            fileInfo = fileRecordService.uploadExcelBytesToDownload(StringConstant.EMPTY_STRING, fileName, excelBytes);
        } catch (Exception e) {
            throw new BusinessException("Error generating Excel {0} with the provided data.", fileName, e);
        }
        // Generate an export history record
        this.generateExportHistory(null, fileInfo.getFileId());
        return fileInfo;
    }

}
