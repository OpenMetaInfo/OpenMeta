package info.openmeta.starter.file.service.impl;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.starter.file.entity.ExportTemplate;
import info.openmeta.starter.file.excel.ExportByDynamic;
import info.openmeta.starter.file.excel.ExportByFileTemplate;
import info.openmeta.starter.file.excel.ExportByTemplate;
import info.openmeta.starter.file.service.ExportService;
import info.openmeta.starter.file.service.ExportTemplateService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.util.Map;

@Service
public class ExportServiceImpl implements ExportService {

    @Autowired
    private ExportByDynamic exportByDynamic;

    @Autowired
    private ExportByTemplate exportByTemplate;

    @Autowired
    private ExportByFileTemplate exportByFileTemplate;

    @Autowired
    private ExportTemplateService exportTemplateService;

    /**
     * Validate the exported by file template configuration.
     *
     * @param modelName the model name to be exported
     * @param exportTemplate the export template to be validated
     */
    protected void validateExportTemplate(String modelName, ExportTemplate exportTemplate) {
        Assert.notNull(exportTemplate, "The export template does not exist.");
        Assert.isTrue(StringUtils.hasText(exportTemplate.getModelName()),
                "The model name in the export template cannot be empty.");
        Assert.isTrue(exportTemplate.getModelName().equals(modelName),
                "The model name in the export template is inconsistent with the model name to be exported");
    }

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
        ExportTemplate exportTemplate = exportTemplateService.readOne(exportTemplateId);
        this.validateExportTemplate(modelName, exportTemplate);
        return exportByTemplate.export(modelName, flexQuery, exportTemplate);
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
        ExportTemplate exportTemplate = exportTemplateService.readOne(exportTemplateId);
        this.validateExportTemplate(modelName, exportTemplate);
        Assert.isTrue(exportTemplate.getFileId() != null, "The export template does not have a file template.");
        return exportByFileTemplate.export(modelName, flexQuery, exportTemplate);
    }
}
