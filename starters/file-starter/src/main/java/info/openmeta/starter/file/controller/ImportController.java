package info.openmeta.starter.file.controller;

import com.fasterxml.jackson.core.type.TypeReference;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.base.utils.JsonMapper;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.file.entity.ImportHistory;
import info.openmeta.starter.file.service.ImportService;
import info.openmeta.starter.file.vo.ImportWizard;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.Map;

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
                                                       @RequestParam(name = "file") MultipartFile file,
                                                       @RequestParam(name = "env") String jsonEnv) {
        String fileName = file.getOriginalFilename();
        Assert.isTrue(StringUtils.hasText(fileName), "File name cannot be empty!");
        Assert.notNull(file, "File cannot be empty!");
        Map<String, Object> env = null;
        if (StringUtils.hasText(jsonEnv)) {
            try {
                env = JsonMapper.stringToObject(jsonEnv, new TypeReference<>() {});
            } catch (Exception e) {
                throw new IllegalArgumentException("The string of environment variables must be in JSON format: {0}", jsonEnv, e);
            }
        }
        return ApiResponse.success(importService.importByTemplate(templateId, file, env));
    }

    /**
     * Import data from the uploaded file and dynamic import settings
     *
     * @return the import result
     */
    @Operation(description = "Import data from the uploaded file")
    @PostMapping(value = "/dynamicImport")
    public ApiResponse<ImportHistory> importWithoutTemplate(@ModelAttribute ImportWizard importWizard) {
        Assert.isTrue(StringUtils.hasText(importWizard.getFileName()), "File name cannot be empty!");
        Assert.notNull(importWizard.getFile(), "File cannot be empty!");
        Assert.notNull(importWizard.getImportRule(), "ImportRule cannot be null.");
        return ApiResponse.success(importService.importByDynamic(importWizard));
    }

}
