package info.openmeta.starter.file.controller;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.domain.FileInfo;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.file.entity.FileRecord;
import info.openmeta.starter.file.service.FileRecordService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * FileRecordController
 */
@Tag(name = "File Record")
@RestController
@RequestMapping("/FileRecord")
public class FileRecordController extends EntityController<FileRecordService, FileRecord, String> {

    /**
     * Get the fileInfo by fileId
     */
    @Operation(description = "Get the fileInfo by fileId")
    @GetMapping(value = "/getByFileId")
    @Parameter(name = "fileId", description = "The id of the file object.")
    public ApiResponse<FileInfo> getByFileId(@RequestParam String fileId) {
        Assert.notBlank(fileId, "fileId cannot be empty.");
        return ApiResponse.success(service.getByFileId(fileId));
    }

}