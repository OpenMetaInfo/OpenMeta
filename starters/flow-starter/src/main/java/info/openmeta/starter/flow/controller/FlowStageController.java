package info.openmeta.starter.flow.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.flow.entity.FlowStage;
import info.openmeta.starter.flow.service.FlowStageService;

/**
 * FlowStage Model Controller
 */
@Tag(name = "FlowStage")
@RestController
@RequestMapping("/FlowStage")
public class FlowStageController extends EntityController<FlowStageService, FlowStage, Long> {

}