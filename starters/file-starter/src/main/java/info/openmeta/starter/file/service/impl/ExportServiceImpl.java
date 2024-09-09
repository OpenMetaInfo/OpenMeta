package info.openmeta.starter.file.service.impl;

import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.starter.file.excel.ExportByDynamic;
import info.openmeta.starter.file.excel.ExportByFileTemplate;
import info.openmeta.starter.file.excel.ExportByTemplate;
import info.openmeta.starter.file.service.ExportService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Map;

@Service
public class ExportServiceImpl implements ExportService {

    @Autowired
    private ExportByDynamic exportByDynamic;

    @Autowired
    private ExportByTemplate exportByTemplate;

    @Autowired
    private ExportByFileTemplate exportByFileTemplate;


    /**
     * Export data by dynamic fields and QueryParams, without export template.
     * The convertType should be set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param modelName the model name to be exported
     * @param flexQuery the flex query to be used for data retrieval
     * @return fileInfo object with download URL
     */
    public FileInfo dynamicExport(String modelName, FlexQuery flexQuery) {
        return exportByDynamic.export(modelName, flexQuery);
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
    public FileInfo dynamicExportMultiSheet(String fileName, Map<String, FlexQuery> flexQueryMap) {
        return exportByDynamic.exportMultiSheet(fileName, flexQueryMap);
    }

    /**
     * Export data by exportTemplate configured exported fields.
     * The convertType should be set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param modelName the model name to be exported
     * @param exportTemplateId the ID of the export template
     * @param flexQuery the flex query to be used for data retrieval
     * @return fileInfo object with download URL
     */
    public FileInfo exportByTemplate(String modelName, Long exportTemplateId, FlexQuery flexQuery) {
        return exportByTemplate.export(modelName, exportTemplateId, flexQuery);
    }

    /**
     * Export one or multiple rows of data by file template.
     * The file template is a template file that contains the variables to be filled in.
     *
     * @param modelName the model name to be exported
     * @param exportTemplateId the ID of the export template
     * @param flexQuery the flexQuery of the exported conditions
     * @return fileInfo object with download URL
     */
    public FileInfo exportByFileTemplate(String modelName, Long exportTemplateId, FlexQuery flexQuery) {
        return exportByFileTemplate.export(modelName, exportTemplateId, flexQuery);
    }
}
