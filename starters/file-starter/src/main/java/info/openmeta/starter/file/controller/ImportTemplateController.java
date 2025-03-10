package info.openmeta.starter.file.controller;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.orm.domain.FileInfo;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.file.entity.ExportTemplate;
import info.openmeta.starter.file.entity.ImportTemplate;
import info.openmeta.starter.file.service.ImportService;
import info.openmeta.starter.file.service.ImportTemplateService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * ImportTemplateController
 */
@Tag(name = "Import Template")
@RestController
@RequestMapping("/ImportTemplate")
public class ImportTemplateController extends EntityController<ImportTemplateService, ImportTemplate, Long> {

    @Autowired
    private ImportService importService;

    /**
     * List all import templates of the specified model
     *
     * @param modelName model name
     * @return list of import templates
     */
    @Operation(summary="listByModel", description = "List all import templates of the specified model")
    @PostMapping(value = "/listByModel")
    public ApiResponse<List<ImportTemplate>> listByModel(@RequestParam String modelName) {
        Filters filters = new Filters().eq(ExportTemplate::getModelName, modelName);
        List<ImportTemplate> templates = service.searchList(filters);
        return ApiResponse.success(templates);
    }

    /**
     * Get the fileInfo of the import template by template ID.
     * The fileInfo contains the download URL.
     *
     * @param id template ID
     * @return import template fileInfo
     */
    @Operation(description = """
            Get the fileInfo of the import template by template ID.
            The fileInfo contains the download URL.""")
    @GetMapping("/getTemplateFile")
    public ApiResponse<FileInfo> getTemplateFile(@RequestParam(name = "id") Long id) {
        return ApiResponse.success(importService.getTemplateFile(id));
    }

}