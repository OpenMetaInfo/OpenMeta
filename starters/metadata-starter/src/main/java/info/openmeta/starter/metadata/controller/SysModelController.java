package info.openmeta.starter.metadata.controller;

import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.metadata.entity.SysModel;
import info.openmeta.starter.metadata.enums.MetadataReloadType;
import info.openmeta.starter.metadata.message.ReloadMetadataProducer;
import info.openmeta.starter.metadata.message.dto.ReloadMetadataMessage;
import info.openmeta.starter.metadata.service.SysModelService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * SysModel Model Controller
 */
@Tag(name = "SysModel")
@RestController
@RequestMapping("/SysModel")
public class SysModelController extends EntityController<SysModelService, SysModel, Long> {

    @Autowired
    private ReloadMetadataProducer reloadMetadataProducer;

    /**
     * Send an inner broadcast to reload the ModelManager of replica containers.
     */
    @Operation(summary = "Reload the data of ModelManager")
    @PostMapping("/reloadModelManager")
    public ApiResponse<Boolean> reloadModelManager() {
        Context context = ContextHolder.cloneContext();
        ReloadMetadataMessage reloadMetadataMessage = new ReloadMetadataMessage("SysModel", MetadataReloadType.RELOAD_MODEL_MANAGER, context);
        reloadMetadataProducer.sendInnerBroadcast(reloadMetadataMessage);
        return ApiResponse.success(true);
    }

 }