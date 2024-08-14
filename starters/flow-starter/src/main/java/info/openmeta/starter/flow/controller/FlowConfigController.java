package info.openmeta.starter.flow.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.flow.entity.FlowConfig;
import info.openmeta.starter.flow.service.FlowConfigService;

/**
 * FlowConfig Model Controller
 */
@Tag(name = "FlowConfig")
@RestController
@RequestMapping("/FlowConfig")
public class FlowConfigController extends EntityController<FlowConfigService, FlowConfig, Long> {

}