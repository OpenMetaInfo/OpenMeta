package info.openmeta.starter.designer.controller;

import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.designer.dto.ModelChangesDTO;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.*;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.designer.entity.DesignAppEnv;
import info.openmeta.starter.designer.service.DesignAppEnvService;

import java.util.List;

/**
 * DesignAppEnv Model Controller
 */
@Tag(name = "DesignAppEnv")
@RestController
@RequestMapping("/DesignAppEnv")
public class DesignAppEnvController extends EntityController<DesignAppEnvService, DesignAppEnv, Long> {

    /**
     * Get the not versioned changes of the specified App env.
     * @param id App env ID
     * @return List of model changed data DTOs
     */
    @Operation(description = "Get the not versioned changes of the App env.")
    @GetMapping(value = "/getNotVersionedChanges")
    @Parameters({
            @Parameter(name = "id", description = "App env ID"),
    })
    public ApiResponse<List<ModelChangesDTO>> getNotVersionedChanges(@RequestParam Long id) {
        return ApiResponse.success(service.getNotVersionedChanges(id));
    }

    /**
     * Preview the version change content between the source and target environments.
     *
     * @param sourceEnvId Source Environment ID
     * @param targetEnvId Target Environment ID
     * @return true / Exception
     */
    @Operation(description = "Preview changes between two environments.")
    @PostMapping(value = "/previewBetweenEnv")
    @Parameters({
            @Parameter(name = "sourceEnvId", description = "Source Environment ID"),
            @Parameter(name = "targetEnvId", description = "Target Environment ID")
    })
    public ApiResponse<List<ModelChangesDTO>> previewBetweenEnv(@RequestParam Long sourceEnvId, @RequestParam Long targetEnvId) {
        service.previewBetweenEnv(sourceEnvId, targetEnvId);
        return ApiResponse.success(service.previewBetweenEnv(sourceEnvId, targetEnvId));
    }

    /**
     * Merge the version change content between the source and target environments.
     *
     * @param sourceEnvId Source Environment ID
     * @param targetEnvId Target Environment ID
     * @return true / Exception
     */
    @Operation(description = "Merge changes between environments.")
    @PostMapping(value = "/mergeBetweenEnv")
    @Parameters({
            @Parameter(name = "sourceEnvId", description = "Source Environment ID"),
            @Parameter(name = "targetEnvId", description = "Target Environment ID")
    })
    public ApiResponse<Boolean> mergeBetweenEnv(@RequestParam Long sourceEnvId, @RequestParam Long targetEnvId) {
        service.mergeBetweenEnv(sourceEnvId, targetEnvId);
        return ApiResponse.success(true);
    }
}