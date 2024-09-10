package info.openmeta.starter.file.excel;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.write.metadata.WriteSheet;
import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.enums.FileType;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.starter.file.entity.ExportTemplate;
import info.openmeta.starter.file.enums.FileSource;
import info.openmeta.starter.file.service.FileRecordService;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Export by file template.
 * Export data based on the uploaded template file
 */
@Slf4j
@Component
public class ExportByFileTemplate extends CommonExport {

    @Autowired
    private FileRecordService fileRecordService;

    /**
     * Export data rows by file template.
     * The file template is a template file that contains the variables to be filled in.
     *
     * @param flexQuery the flexQuery of the exported conditions
     * @param exportTemplate exportTemplate object
     * @return fileInfo object with download URL
     */
    public FileInfo export(FlexQuery flexQuery, ExportTemplate exportTemplate) {
        // TODO: cache the extracted fields in the exportTemplate
        Set<String> fields = extractVariablesOfFileTemplate(exportTemplate.getFileId());
        flexQuery.setFields(fields);
        List<Map<String, Object>> rows = this.getExportedData(exportTemplate.getModelName(), flexQuery);
        // Fill in the data into the file template
        FileInfo fileInfo = this.generateByFileTemplateAndUpload(exportTemplate, rows);
        // Generate export history
        this.generateExportHistory(exportTemplate.getId(), fileInfo.getFileId());
        return fileInfo;
    }

    /**
     * Renders data into a file template and uploads the generated file to OSS.
     *
     * @param exportTemplate the export template object
     * @param data the data to be filled into the file template
     * @return fileInfo object with download URL
     */
    private FileInfo generateByFileTemplateAndUpload(ExportTemplate exportTemplate, Object data) {
        String fileName = exportTemplate.getFileName();
        String sheetName = StringUtils.hasText(exportTemplate.getSheetName()) ? exportTemplate.getSheetName() : fileName;
        try (InputStream inputStream = fileRecordService.downloadStream(exportTemplate.getFileId());
             ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
             // Use EasyExcel to write the template and fill in the data
             ExcelWriter excelWriter = EasyExcel.write(outputStream).withTemplate(inputStream).build()) {
            // Create a write sheet and fill in the data
            WriteSheet writeSheet = EasyExcel.writerSheet(sheetName).build();
            excelWriter.fill(data, writeSheet);
            // TODO: fill in the ENV related to current user
            excelWriter.finish();
            // Convert ByteArrayOutputStream to InputStream for return and upload
            InputStream resultStream = new ByteArrayInputStream(outputStream.toByteArray());
            return fileRecordService.uploadFile(fileName, FileType.XLSX, resultStream, FileSource.DOWNLOAD);
        } catch (Exception e) {
            throw new BusinessException("Failed to fill data into the file template {}.", fileName, e);
        }
    }

    /**
     * Get all the variable parameters in the file template.
     *
     * @param fileId the ID of the file template
     * @return all variables in the template
     */
    private Set<String> extractVariablesOfFileTemplate(Long fileId) {
        try (InputStream inputStream = fileRecordService.downloadStream(fileId);
             Workbook workbook = new XSSFWorkbook(inputStream)) {
            return getVariablesInWorkbook(workbook);
        } catch (IOException e) {
            throw new BusinessException("Failed to read the file template.", e);
        }
    }

    /**
     * Get all the variable parameters in the Workbook (format: {variable})
     * Cases of variable extraction:
     *      - `{name}` -> `name`
     *      - `{  name }` -> `name`
     *      - `Hello, {name123}!` -> `name123`
     *      - `{name}, {orderNumber}` -> `name`, `orderNumber`
     *      - `{deptId.name}` -> `deptId.name`
     *      - `Hello, {name}, {}` -> `name`
     *      - `Hello, {name}, \{id\}` -> `name`
     *
     * @param workbook workbook object
     * @return all variables in the template
     */
    private static Set<String> getVariablesInWorkbook(Workbook workbook) {
        Set<String> variables = new HashSet<>();
        // Iterate over all sheets
        for(Sheet sheet : workbook) {
            // Iterate over all rows in each sheet
            for(Row row : sheet) {
                // Iterate over all cells in each row
                for(Cell cell : row) {
                    if(cell.getCellType() == CellType.STRING) {
                        String cellValue = cell.getStringCellValue();
                        // Use regular expressions to match the variable format ${variable}
                        Pattern pattern = Pattern.compile("\\{\\s*([a-zA-Z0-9_.]+)\\s*}");
                        Matcher matcher = pattern.matcher(cellValue);
                        while(matcher.find()) {
                            variables.add(matcher.group());
                        }
                    }
                }
            }
        }
        return variables;
    }

}
