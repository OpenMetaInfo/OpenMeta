package info.openmeta.starter.cron.controller;

import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.cron.entity.SysCron;
import info.openmeta.starter.cron.service.SysCronService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * SysCron Model Controller
 */
@Tag(name = "SysCron")
@RestController
@RequestMapping("/SysCron")
public class SysCronController extends EntityController<SysCronService, SysCron, Long> {

    @Operation(summary = "Execute once now", description = "Immediately run the specified cron job for once.")
    @PostMapping(value = "/executeNow")
    @Parameter(name = "id", description = "Cron Job ID")
    public ApiResponse<Boolean> executeNow(@RequestParam Long id) {
        service.executeNow(id);
        return ApiResponse.success(true);
    }

    @Operation(summary = "Execute multiple for once now",
            description = "Immediately run the specified multiple cron jobs for once.")
    @PostMapping(value = "/executeMultipleNow")
    @Parameter(name = "ids", description = "Cron Job IDs")
    public ApiResponse<Boolean> executeMultipleNow(@RequestParam List<Long> ids) {
        service.executeMultipleNow(ids);
        return ApiResponse.success(true);
    }
}