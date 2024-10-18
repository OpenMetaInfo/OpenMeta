package info.openmeta.starter.metadata.controller;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.meta.MetaOptionItem;
import info.openmeta.framework.orm.meta.OptionManager;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.metadata.entity.SysOptionSet;
import info.openmeta.starter.metadata.service.SysOptionSetService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * SysOptionSet Model Controller
 */
@Tag(name = "SysOptionSet")
@RestController
@RequestMapping("/SysOptionSet")
public class SysOptionSetController extends EntityController<SysOptionSetService, SysOptionSet, Long> {

    /**
     * Get the option set items of the specified option set code.
     *
     * @param optionSetCode option set code
     * @return option set items
     */
    @GetMapping("/getOptionSetItems")
    @Operation(summary = "getOptionSetItems", description = "Get the option set items of the specified option set code.")
    @Parameter(name = "optionSetCode", description = "Option set code", required = true)
    public ApiResponse<List<MetaOptionItem>> getOptionSetItems(String optionSetCode) {
        Assert.notBlank(optionSetCode, "Option set code cannot be empty.");
        return ApiResponse.success(OptionManager.getMetaOptionItems(optionSetCode));
    }
}