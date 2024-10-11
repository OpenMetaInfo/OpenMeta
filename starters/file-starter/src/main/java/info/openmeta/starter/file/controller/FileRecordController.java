package info.openmeta.starter.file.controller;

import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.file.entity.FileRecord;
import info.openmeta.starter.file.service.FileRecordService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.util.Assert;
import org.springframework.web.bind.annotation.*;

/**
 * FileRecordController
 */
@Tag(name = "File Record")
@RestController
@RequestMapping("/FileRecord")
public class FileRecordController extends EntityController<FileRecordService, FileRecord, Long> {

    /**
     * Get the fileInfo by fileId
     */
    @Operation(description = "Get the fileInfo by fileId")
    @GetMapping(value = "/getFileInfo")
    @Parameter(name = "fileId", description = "The id of the file object.")
    public ApiResponse<FileInfo> getFileInfo(@RequestParam Long fileId) {
        Assert.notNull(fileId, "fileId cannot be null.");
        return ApiResponse.success(service.getFileInfo(fileId));
    }

}