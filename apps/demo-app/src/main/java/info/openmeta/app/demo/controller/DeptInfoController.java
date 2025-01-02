package info.openmeta.app.demo.controller;

import info.openmeta.app.demo.entity.DeptInfo;
import info.openmeta.app.demo.service.DeptInfoService;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.orm.annotation.DataMask;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.web.response.ApiResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDate;
import java.util.List;

/**
 * DeptInfo Model Controller
 */
@Tag(name = "DeptInfo")
@RestController
@RequestMapping("/DeptInfo")
public class DeptInfoController extends EntityController<DeptInfoService, DeptInfo, Long> {

    @Autowired
    private DeptInfoService deptInfoService;

    @GetMapping(value = "/readCustomize")
    @Operation(summary = "readCustomize", description = "Read customized department info by ID.")
    @Parameters({
            @Parameter(name = "id", description = "Data ID, number or string type.", schema = @Schema(type = "number")),
            @Parameter(name = "fields", description = "A list of field names to be read. If not specified, it defaults to all visible fields."),
            @Parameter(name = "effectiveDate", description = "Effective date for timeline model.")
    })
    @DataMask
    public ApiResponse<DeptInfo> readOne(@RequestParam Long id,
                                         @RequestParam(required = false) List<String> fields,
                                         @RequestParam(required = false) LocalDate effectiveDate) {
        ContextHolder.getContext().setEffectiveDate(effectiveDate);
        return ApiResponse.success(deptInfoService.getById(id, fields).orElse(null));
    }

}