package info.openmeta.starter.flow.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.flow.entity.FlowInstance;
import info.openmeta.starter.flow.service.FlowInstanceService;

/**
 * FlowInstance Model Controller
 */
@Tag(name = "FlowInstance")
@RestController
@RequestMapping("/FlowInstance")
public class FlowInstanceController extends EntityController<FlowInstanceService, FlowInstance, String> {

}