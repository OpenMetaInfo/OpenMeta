package info.openmeta.starter.metadata.controller;

import info.openmeta.framework.base.enums.SystemUser;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.annotation.SwitchUser;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.metadata.entity.SysPreData;
import info.openmeta.starter.metadata.service.SysPreDataService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * SysPreData Model Controller
 */
@Tag(name = "SysPreData")
@RestController
@RequestMapping("/SysPreData")
public class SysPreDataController extends EntityController<SysPreDataService, SysPreData, Long> {

    /**
     * Load the specified list of predefined data files from the root directory: resources/data.
     * Supports data files in JSON, XML, and CSV formats.
     * @return success or not
     */
    @Operation(summary = "loadData", description = """
            Load the specified list of predefined data files from the root directory resources/data,
            supporting data files in JSON, XML, and CSV formats.
            """)
    @PostMapping("/loadData")
    public ApiResponse<Boolean> loadPredefinedData(@RequestBody List<String> fileNames) {
        Assert.allNotBlank(fileNames, "The filename of the data to be loaded cannot be empty!");
        service.loadPredefinedData(fileNames);
        return ApiResponse.success(true);
    }


    /**
     * Upload a predefined data file to load data.
     *
     * @param file the multipart file containing the predefined data to be loaded
     * @return success or not
     */
    @Operation(summary = "uploadFile")
    @PostMapping("/uploadFile")
    @SwitchUser(value = SystemUser.INTEGRATION_USER)
    public ApiResponse<Boolean> uploadPredefinedData(@RequestParam("file") MultipartFile file) {
        Assert.notTrue(file.isEmpty(), "The file to be uploaded cannot be empty!");
        service.loadPredefinedData(file);
        return ApiResponse.success(true);
    }
}