package info.openmeta.starter.file.excel;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.write.metadata.WriteSheet;
import info.openmeta.framework.base.constant.StringConstant;
import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.orm.domain.FileInfo;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.utils.ListUtils;
import info.openmeta.starter.file.dto.ExcelDataDTO;
import info.openmeta.starter.file.dto.SheetInfo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Export by dynamic parameters
 */
@Slf4j
@Component
public class ExportByDynamic extends CommonExport {

    /**
     * Export data by dynamic fields and QueryParams, without export template.
     * The convertType should be set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param modelName the model name to be exported
     * @param fileName the name of the Excel file to be generated
     * @param sheetName the name of the sheet in the Excel file
     * @param flexQuery the flex query to be used for data retrieval
     * @return fileInfo object with download URL
     */
    public FileInfo export(String modelName, String fileName, String sheetName, FlexQuery flexQuery) {
        // Get the data to be exported
        List<String> headers = new ArrayList<>();
        List<List<Object>> rowsTable = this.extractDataTableFromDB(modelName, flexQuery, headers);
        // Generate the Excel file
        String modelLabel = ModelManager.getModel(modelName).getLabelName();
        fileName = StringUtils.hasText(fileName) ? fileName : modelLabel;
        sheetName = StringUtils.hasText(sheetName) ? sheetName : fileName;
        // Excel data DTO
        ExcelDataDTO excelDataDTO = new ExcelDataDTO();
        excelDataDTO.setFileName(fileName);
        excelDataDTO.setSheetName(sheetName);
        excelDataDTO.setHeaders(headers);
        excelDataDTO.setRowsTable(rowsTable);
        FileInfo fileInfo = this.generateFileAndUpload(modelName, excelDataDTO);
        // Generate an export history record
        this.generateExportHistory(null, fileInfo.getFileId());
        return fileInfo;
    }

    /**
     * Export multiple sheets of data by dynamic fields and QueryParams, without export template.
     * The convertType should be set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param fileName the name of the Excel file to be exported
     * @param sheetInfoList the list of sheetInfo objects
     * @return fileInfo object with download URL
     */
    public FileInfo exportMultiSheet(String fileName, List<SheetInfo> sheetInfoList) {
        FileInfo fileInfo;
        // Generate the Excel file
        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
             // Use EasyExcel to write the file with dynamic headers and data
             ExcelWriter excelWriter = EasyExcel.write(outputStream).build()) {
            for (int i = 0; i < sheetInfoList.size(); i++) {
                SheetInfo sheetInfo = sheetInfoList.get(i);
                List<String> headers = new ArrayList<>();
                List<List<Object>> rowsTable = this.extractDataTableFromDB(sheetInfo.getModelName(), sheetInfo.getFlexQuery(), headers);
                // Write the header and data, EasyExcel requires the header to be a list of lists
                List<List<String>> headerList = headers.stream().map(Collections::singletonList).toList();
                String sheetName = StringUtils.hasText(sheetInfo.getSheetName()) ? sheetInfo.getSheetName() : sheetInfo.getModelName();
                WriteSheet writeSheet = EasyExcel.writerSheet(i, sheetName).head(headerList).build();
                excelWriter.write(rowsTable, writeSheet);
            }
            excelWriter.finish();
            // upload the Excel bytes to the file storage
            byte[] excelBytes = outputStream.toByteArray();
            fileInfo = this.uploadExcelBytes(StringConstant.EMPTY_STRING, fileName, excelBytes);
        } catch (Exception e) {
            throw new BusinessException("Error generating Excel {0} with the provided data.", fileName, e);
        }
        // Generate an export history record
        this.generateExportHistory(null, fileInfo.getFileId());
        return fileInfo;
    }

    /**
     * Extract the data table from the database by the given model name and flexQuery.
     * And extract the header list from the model fields.
     *
     * @param modelName the model name to be exported
     * @param flexQuery the flexQuery object
     * @param headers the list of header label
     */
    private List<List<Object>> extractDataTableFromDB(String modelName, FlexQuery flexQuery, List<String> headers) {
        // Get the data to be exported
        List<Map<String, Object>> rows = this.getExportedRows(modelName, null, flexQuery);
        // Construct the headers order by sequence of the fields
        List<String> fieldNames = flexQuery.getFields();
        fieldNames.forEach(fieldName -> {
            MetaField lastField = ModelManager.getLastFieldOfCascaded(modelName, fieldName);
            headers.add(lastField.getLabelName());
        });
        return ListUtils.convertToTableData(fieldNames, rows);
    }
}
