package info.openmeta.starter.file.excel;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.write.metadata.WriteSheet;
import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.enums.FileType;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.utils.ListUtils;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.starter.file.enums.FileSource;
import info.openmeta.starter.file.service.FileRecordService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
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

    @Autowired
    private FileRecordService fileRecordService;

    /**
     * Export data by dynamic fields and QueryParams, without export template.
     * The convertType should be set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param modelName the model name to be exported
     * @param flexQuery the flex query to be used for data retrieval
     * @return fileInfo object with download URL
     */
    public FileInfo export(String modelName, FlexQuery flexQuery) {
        // Get the data to be exported
        List<List<String>> headerList = new ArrayList<>();
        List<List<Object>> rowsTable = this.extractDataTableFromDB(modelName, flexQuery, headerList);
        // Generate the Excel file
        String modelLabel = ModelManager.getModel(modelName).getLabelName();
        FileInfo fileInfo = this.generateFileAndUpload(modelLabel, headerList, rowsTable);
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
     * @param flexQueryMap the map of {modelName: flexQuery}
     * @return fileInfo object with download URL
     */
    public FileInfo exportMultiSheet(String fileName, Map<String, FlexQuery> flexQueryMap) {
        FileInfo fileInfo;
        // Generate the Excel file
        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
             // Use EasyExcel to write the file with dynamic headers and data
             ExcelWriter excelWriter = EasyExcel.write(outputStream).build()) {
            for (Map.Entry<String, FlexQuery> entry : flexQueryMap.entrySet()) {
                List<List<String>> headerList = new ArrayList<>();
                List<List<Object>> rowsTable = this.extractDataTableFromDB(entry.getKey(), entry.getValue(), headerList);
                // Write the header and data
                WriteSheet writeSheet = EasyExcel.writerSheet().head(headerList).build();
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
     * Extract the data table from the database by the given model name and flexQuery.
     * And extract the header list from the model fields.
     *
     * @param modelName the model name to be exported
     * @param flexQuery the flexQuery object
     * @param headerList the list of header labels
     */
    private List<List<Object>> extractDataTableFromDB(String modelName, FlexQuery flexQuery, List<List<String>> headerList) {
        // Construct the headers order by sequence of the fields
        List<String> fieldNames = flexQuery.getFields();
        fieldNames.forEach(fieldName -> {
            String labelName = ModelManager.getCascadingFieldLabelName(modelName, fieldName);
            headerList.add(Collections.singletonList(labelName));
        });
        // Get the data to be exported
        List<Map<String, Object>> rows = this.getExportedData(modelName, flexQuery);
        return ListUtils.convertToTableData(fieldNames, rows);
    }
}
