package info.openmeta.starter.flow.controller;

import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.flow.entity.FlowConfig;
import info.openmeta.starter.flow.service.FlowConfigService;
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

import java.util.List;
import java.util.Map;

/**
 * FlowConfig Model Controller
 */
@Tag(name = "FlowConfig")
@RestController
@RequestMapping("/FlowConfig")
public class FlowConfigController extends EntityController<FlowConfigService, FlowConfig, Long> {

    @Autowired
    private ModelService<?> modelService;

    /**
     * Get the flow list by model name.
     *
     * @param modelName model name
     * @param fields field list to be read
     * @return flow configuration list
     */
    @GetMapping(value = "/getByModel")
    @Operation(summary = "getByModel", description = "Get flow list by model.")
    @Parameters({
            @Parameter(name = "modelName", description = "The model name of flow", schema = @Schema(type = "string")),
            @Parameter(name = "fields", description = "A list of field names to be read."),
    })
    public ApiResponse<List<Map<String, Object>>> getByModel(@RequestParam String modelName,
                                                             @RequestParam(required = false) List<String> fields) {
        Assert.isTrue(ModelManager.existModel(modelName), "Model {} not found", modelName);
        return ApiResponse.success(service.getByModel(modelName));
    }

    @GetMapping(value = "/getFlowById")
    @Operation(summary = "getFlowById", description = "Get flow config by ID.")
    @Parameter(name = "id", description = "The flow ID", schema = @Schema(type = "number"))
    public ApiResponse<FlowConfig> getFlowById(@RequestParam Long id) {
        FlowConfig flowConfig = service.getFlowById(id)
                .orElseThrow(() -> new IllegalArgumentException("FlowConfig not found by ID: {0}", id));
        return ApiResponse.success(flowConfig);
    }
}