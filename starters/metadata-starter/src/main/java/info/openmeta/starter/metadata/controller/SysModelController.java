package info.openmeta.starter.metadata.controller;

import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.meta.MetaField;
import info.openmeta.framework.orm.meta.ModelManager;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.metadata.entity.SysModel;
import info.openmeta.starter.metadata.service.SysModelService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collection;

/**
 * SysModel Model Controller
 */
@Tag(name = "SysModel")
@RestController
@RequestMapping("/SysModel")
public class SysModelController extends EntityController<SysModelService, SysModel, Long> {

    /**
     * Get the SysField objects of the model.
     *
     * @param modelName model name
     * @return SysField objects of the model
     */
    @GetMapping("/getModelFields")
    @Operation(summary = "getModelFields", description = "Get the fields of the model.")
    @Parameter(name = "modelName", description = "Model name", required = true)
    public ApiResponse<Collection<MetaField>> getModelFields(String modelName) {
        Assert.notBlank(modelName, "Model name cannot be empty.");
        return ApiResponse.success(ModelManager.getModelFields(modelName));
    }
}