package info.openmeta.starter.file.service;

import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.starter.file.dto.SheetInfo;

import java.util.List;

public interface ExportService {

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
    FileInfo dynamicExport(String modelName, String fileName, String sheetName, FlexQuery flexQuery);

    /**
     * Export multiple sheets of data by dynamic fields and QueryParams, without export template.
     * The convertType should be set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param fileName the name of the Excel file to be exported
     * @param sheetInfoList the list of sheetInfo objects
     * @return fileInfo object with download URL
     */
    FileInfo dynamicExportMultiSheet(String fileName, List<SheetInfo> sheetInfoList);

    /**
     * Export data by exportTemplate configured exported fields.
     * The convertType should be set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param exportTemplateId the ID of the export template
     * @param flexQuery the flex query to be used for data retrieval
     * @return fileInfo object with download URL
     */
    FileInfo exportByTemplate(Long exportTemplateId, FlexQuery flexQuery);

    /**
     * Export multiple sheets merged to on Excel file by specifying multi export templates.
     * The convertType should be set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param fileName the name of the Excel file to be exported
     * @param ids the list of export template id
     * @return fileInfo object with download URL
     */
    FileInfo exportByMultiTemplate(String fileName, List<Long> ids);

    /**
     * Export one or multiple rows of data by file template.
     * The file template is a template file that contains the variables to be filled in.
     *
     * @param exportTemplateId the ID of the export template
     * @param flexQuery the flexQuery of the exported conditions
     * @return fileInfo object with download URL
     */
    FileInfo exportByFileTemplate(Long exportTemplateId, FlexQuery flexQuery);
}
