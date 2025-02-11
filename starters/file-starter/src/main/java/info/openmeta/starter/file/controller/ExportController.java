package info.openmeta.starter.file.controller;

import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.file.service.ExportService;
import info.openmeta.starter.file.vo.ExportParams;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.Assert;
import org.springframework.web.bind.annotation.*;

/**
 * ExportController
 */
@Tag(name = "Data Export")
@RestController
@RequestMapping("/export")
public class ExportController {

    @Autowired
    private ExportService exportService;

    /**
     * Export data by dynamic fields and ExportParams, without export template.
     * The convertType is set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param modelName the model name to be exported
     * @param fileName the name of the Excel file to be generated
     * @param sheetName the name of the sheet in the Excel file
     * @param exportParams the export parameters of the data to be exported
     * @return fileInfo object with download URL
     */
    @Operation(description = "Export data by dynamic fields and ExportParams, without export template.")
    @PostMapping(value = "/dynamicExport")
    public ApiResponse<FileInfo> dynamicExport(@RequestParam String modelName,
                                               @RequestParam(required = false) String fileName,
                                               @RequestParam(required = false) String sheetName,
                                               @RequestBody ExportParams exportParams) {
        FlexQuery flexQuery = ExportParams.convertParamsToFlexQuery(exportParams);
        return ApiResponse.success(exportService.dynamicExport(modelName, fileName, sheetName, flexQuery));
    }

    /**
     * Export data by exportTemplate configured exported fields.
     * The convertType is set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param exportTemplateId The ID of the export template
     * @param exportParams the export parameters of the data to be exported
     * @return fileInfo object with download URL
     */
    @Operation(description = "Exported by exportTemplate with dynamic fields and ExportParams")
    @PostMapping(value = "/exportByTemplate")
    @Parameter(name = "exportTemplateId", description = "The id of the ExportTemplate.")
    public ApiResponse<FileInfo> exportByTemplate(@RequestParam Long exportTemplateId,
                                                  @RequestBody ExportParams exportParams) {
        FlexQuery flexQuery = ExportParams.convertParamsToFlexQuery(exportParams);
        return ApiResponse.success(exportService.exportByTemplate(exportTemplateId, flexQuery));
    }

    /**
     * Export data by file template.
     * The file template is a template file that contains the variables to be filled in.
     * The convertType is set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param exportTemplateId the ID of the export template
     * @param exportParams the export parameters of the data to be exported
     * @return fileInfo object with download URL
     */
    @Operation(description = "Exported by file template and ExportParams")
    @PostMapping(value = "/exportByFileTemplate")
    @Parameter(name = "exportTemplateId", description = "The id of the ExportTemplate.")
    public ApiResponse<FileInfo> exportByFileTemplate(@RequestParam Long exportTemplateId,
                                                      @RequestBody ExportParams exportParams) {
        Assert.notNull(exportTemplateId, "Export template ID cannot be null.");
        FlexQuery flexQuery = ExportParams.convertParamsToFlexQuery(exportParams);
        return ApiResponse.success(exportService.exportByFileTemplate(exportTemplateId, flexQuery));
    }

}
