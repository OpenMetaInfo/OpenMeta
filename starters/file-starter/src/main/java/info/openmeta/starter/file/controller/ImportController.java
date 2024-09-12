package info.openmeta.starter.file.controller;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.file.entity.ImportHistory;
import info.openmeta.starter.file.service.ImportService;
import info.openmeta.starter.file.vo.ImportFileVO;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

/**
 * ImportController
 */
@Tag(name = "Import")
@RestController
@RequestMapping("/import")
public class ImportController {

    @Autowired
    private ImportService importService;

    /**
     * Import data from the uploaded file and the import template ID
     *
     * @param templateId       the ID of the import template
     * @param file             the uploaded file
     * @return the import result
     */
    @Operation(description = "Import data from the uploaded file")
    @PostMapping(value = "/importByTemplate")
    public ApiResponse<ImportHistory> importByTemplate(@RequestParam(name = "templateId") Long templateId,
                                                       @RequestParam(name = "file") MultipartFile file) {
        String fileName = file.getOriginalFilename();
        Assert.isTrue(StringUtils.hasText(fileName), "File name cannot be empty!");
        return ApiResponse.success(importService.importByTemplate(templateId, file));
    }

    /**
     * Import data from the uploaded file and dynamic import settings
     *
     * @return the import result
     */
    @Operation(description = "Import data from the uploaded file")
    @PostMapping(value = "/dynamicImport")
    public ApiResponse<ImportHistory> importWithoutTemplate(@ModelAttribute ImportFileVO importFileVO) {
        Assert.isTrue(StringUtils.hasText(importFileVO.getFileName()), "File name cannot be empty!");
        return ApiResponse.success(importService.importByDynamic(importFileVO));
    }

}
