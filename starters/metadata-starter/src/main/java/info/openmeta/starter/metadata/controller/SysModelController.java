package info.openmeta.starter.metadata.controller;

import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.metadata.entity.SysModel;
import info.openmeta.starter.metadata.service.SysModelService;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * SysModel Model Controller
 */
@Tag(name = "SysModel")
@RestController
@RequestMapping("/SysModel")
public class SysModelController extends EntityController<SysModelService, SysModel, Long> {

}