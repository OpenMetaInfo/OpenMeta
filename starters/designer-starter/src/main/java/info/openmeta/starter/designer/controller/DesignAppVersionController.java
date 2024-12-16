package info.openmeta.starter.designer.controller;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.annotation.DataMask;
import info.openmeta.framework.orm.utils.BeanTool;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.designer.entity.DesignAppVersion;
import info.openmeta.starter.designer.service.DesignAppVersionService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/**
 * DesignAppVersion Model Controller
 */
@Tag(name = "DesignAppVersion")
@RestController
@RequestMapping("/DesignAppVersion")
public class DesignAppVersionController extends EntityController<DesignAppVersionService, DesignAppVersion, Long> {

    /**
     * Create a new App version.
     *
     * @param row       data row to be created
     * @return id
     */
    @PostMapping(value = "/createOne")
    @Operation(description = "Create one row and return the id.")
    @DataMask
    public ApiResponse<Long> createOne(@RequestBody Map<String, Object> row) {
        DesignAppVersion appVersion = BeanTool.mapToObject(row, DesignAppVersion.class);
        appVersion.setId(null);
        Assert.notBlank(appVersion.getName(), "Version name is required.");
        Assert.notNull(appVersion.getEnvId(), "Env ID is required.");
        return ApiResponse.success(service.createOne(appVersion));
    }

    /**
     * Reload App env changes to current version.
     *
     * @param id Version ID
     */
    @Operation(description = "Reload App env changes to current version.")
    @PostMapping(value = "/reloadAppVersion")
    @Parameter(name = "id", description = "Version ID")
    public ApiResponse<Boolean> reloadAppVersion(@RequestParam Long id) {
        return ApiResponse.success(service.reloadAppVersion(id));
    }

    /**
     * Publish the version to the target environment.
     *
     * @param id Version ID
     * @return true / Exception
     */
    @Operation(description = "Publish the version to the target environment.")
    @PostMapping(value = "/publish")
    @Parameter(name = "id", description = "Version ID")
    public ApiResponse<Boolean> publish(@RequestParam Long id) {
        service.publish(id);
        return ApiResponse.success(true);
    }

}