package info.openmeta.starter.file.controller;

import info.openmeta.framework.orm.domain.Filters;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.file.entity.ExportTemplate;
import info.openmeta.starter.file.service.ExportTemplateService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * ExportTemplateController
 */
@Tag(name = "Export Template")
@RestController
@RequestMapping("/ExportTemplate")
public class ExportTemplateController extends EntityController<ExportTemplateService, ExportTemplate, Long> {

    @Autowired
    private ExportTemplateService exportTemplateService;

    /**
     * List all export templates of the specified model
     *
     * @param modelName model name
     * @return list of export templates
     */
    @Operation(description = "List all export templates of the specified model")
    @PostMapping(value = "/listByModel")
    public ApiResponse<List<ExportTemplate>> listByModel(@RequestParam String modelName) {
        Filters filters = Filters.eq(ExportTemplate::getModelName, modelName);
        List<ExportTemplate> templates = exportTemplateService.searchList(filters);
        return ApiResponse.success(templates);
    }
}