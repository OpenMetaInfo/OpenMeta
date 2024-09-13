package info.openmeta.starter.file.controller;

import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.orm.domain.FlexQuery;
import info.openmeta.framework.orm.domain.QueryParams;
import info.openmeta.framework.orm.enums.ConvertType;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.file.service.ExportService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
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
     * Export data by dynamic fields and QueryParams, without export template.
     * The convertType is set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param modelName the model name to be exported
     * @param fileName the name of the Excel file to be generated
     * @param sheetName the name of the sheet in the Excel file
     * @param queryParams the query parameters of the data to be exported
     * @return fileInfo object with download URL
     */
    @Operation(description = "Export data by dynamic fields and QueryParams, without export template.")
    @PostMapping(value = "/dynamicExport")
    public ApiResponse<FileInfo> dynamicExport(@RequestParam String modelName,
                                               @RequestParam(required = false) String fileName,
                                               @RequestParam(required = false) String sheetName,
                                               @RequestBody QueryParams queryParams) {
        ContextHolder.getContext().setEffectiveDate(queryParams.getEffectiveDate());
        FlexQuery flexQuery = new FlexQuery(queryParams.getFields(), queryParams.getFilters(), queryParams.getOrders());
        flexQuery.setConvertType(ConvertType.DISPLAY);
        if (!CollectionUtils.isEmpty(queryParams.getGroupBy())) {
            flexQuery.setGroupBy(queryParams.getGroupBy());
        }
        return ApiResponse.success(exportService.dynamicExport(modelName, fileName, sheetName, flexQuery));
    }

    /**
     * Export data by exportTemplate configured exported fields.
     * The convertType is set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param exportTemplateId The ID of the export template
     * @return fileInfo object with download URL
     */
    @Operation(description = "Exported by exportTemplate with dynamic fields and QueryParams")
    @PostMapping(value = "/exportByTemplate")
    @Parameter(name = "exportTemplateId", description = "The id of the ExportTemplate.")
    public ApiResponse<FileInfo> exportByTemplate(@RequestParam Long exportTemplateId,
                                                  @RequestBody QueryParams queryParams) {
        ContextHolder.getContext().setEffectiveDate(queryParams.getEffectiveDate());
        FlexQuery flexQuery = new FlexQuery(queryParams.getFilters(), queryParams.getOrders());
        flexQuery.setConvertType(ConvertType.DISPLAY);
        if (!CollectionUtils.isEmpty(queryParams.getGroupBy())) {
            flexQuery.setGroupBy(queryParams.getGroupBy());
        }
        return ApiResponse.success(exportService.exportByTemplate(exportTemplateId, flexQuery));
    }

    /**
     * Export data by file template.
     * The file template is a template file that contains the variables to be filled in.
     * The convertType is set to DISPLAY to get the display values of the fields.
     * Such as displayName for ManyToOne/OneToOne fields, and itemName for Option fields.
     *
     * @param exportTemplateId the ID of the export template
     * @param queryParams the query parameters of the data to be exported
     * @return fileInfo object with download URL
     */
    @Operation(description = "Exported by file template and QueryParams")
    @PostMapping(value = "/exportByFileTemplate")
    @Parameter(name = "exportTemplateId", description = "The id of the ExportTemplate.")
    public ApiResponse<FileInfo> exportByFileTemplate(@RequestParam Long exportTemplateId,
                                                      @RequestBody QueryParams queryParams) {
        Assert.notNull(exportTemplateId, "Export template ID cannot be null.");
        ContextHolder.getContext().setEffectiveDate(queryParams.getEffectiveDate());
        FlexQuery flexQuery = new FlexQuery(queryParams.getFilters(), queryParams.getOrders());
        flexQuery.setConvertType(ConvertType.DISPLAY);
        if (!CollectionUtils.isEmpty(queryParams.getGroupBy())) {
            flexQuery.setGroupBy(queryParams.getGroupBy());
        }
        return ApiResponse.success(exportService.exportByFileTemplate(exportTemplateId, flexQuery));
    }

}
