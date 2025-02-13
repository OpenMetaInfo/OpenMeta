package info.openmeta.starter.file.controller;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.domain.FileInfo;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.file.entity.FileRecord;
import info.openmeta.starter.file.service.FileRecordService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.Serializable;
import java.util.List;

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

    /**
     * Get the fileInfo by modelName and rowId
     */
    @Operation(description = "Get the fileInfo by modelName and rowId")
    @GetMapping(value = "/getFileInfo")
    @Parameter(name = "fileId", description = "The id of the file object.")
    public ApiResponse<List<FileInfo>> getFileInfo(@RequestParam String modelName,
                                                   @RequestParam Serializable rowId) {
        Assert.notBlank(modelName, "modelName cannot be empty.");
        Assert.notNull(rowId, "rowId cannot be null.");
        return ApiResponse.success(service.getFileInfo(modelName, rowId));
    }

    /**
     * Upload a file to the specified model and row, and return the fileInfo.
     *
     * @param modelName The model name of the file belongs to
     * @param rowId The row ID of the file belongs to
     * @param fieldName The field name of the file belongs to
     * @param file The file to be uploaded
     * @return The fileInfo of the uploaded file
     */
    @Operation(summary = "uploadFile")
    @PostMapping("/uploadFile")
    @Parameters({
            @Parameter(name = "modelName", description = "The model name of the file belongs to"),
            @Parameter(name = "rowId", description = "The row ID of the file belongs to"),
            @Parameter(name = "fieldName", description = "The field name of the file belongs to"),
            @Parameter(name = "file", description = "The file to be uploaded")
    })
    public ApiResponse<FileInfo> uploadPredefinedData(@RequestParam String modelName,
                                                     @RequestParam Serializable rowId,
                                                     @RequestParam(required = false) String fieldName,
                                                     @RequestParam MultipartFile file) {
        Assert.notBlank(modelName, "The modelName cannot be empty!");
        Assert.notNull(rowId, "The rowId cannot be null!");
        Assert.notTrue(file.isEmpty(), "The file to upload cannot be empty!");
        return ApiResponse.success(service.uploadFile(modelName, rowId, fieldName, file));
    }

}