package info.openmeta.starter.file.controller;

import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.file.entity.DocumentTemplate;
import info.openmeta.starter.file.service.DocumentTemplateService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.io.Serializable;

/**
 * DocumentTemplate Model Controller
 */
@Tag(name = "DocumentTemplate")
@RestController
@RequestMapping("/DocumentTemplate")
public class DocumentTemplateController extends EntityController<DocumentTemplateService, DocumentTemplate, Long> {

    /**
     * Generate a word or PDF document according to the specified template ID and row ID.
     *
     * @param templateId template ID
     * @param rowId row ID
     * @return generated document fileInfo with download URL
     */
    @Operation(description = "Generate a word or PDF document according to the specified template ID and row ID.")
    @GetMapping("/generateDocument")
    @Parameters({
            @Parameter(name = "templateId", description = "Template ID"),
            @Parameter(name = "rowId", description = "Data ID of the business data model")
    })
    public ApiResponse<FileInfo> generateDocument(Long templateId, Serializable rowId) {
        FileInfo fileInfo = service.generateDocument(templateId, rowId);
        return ApiResponse.success(fileInfo);
    }

}