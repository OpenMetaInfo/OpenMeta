package info.openmeta.starter.flow.controller;

import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.flow.entity.FlowNode;
import info.openmeta.starter.flow.service.FlowNodeService;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * FlowNode Model Controller
 */
@Tag(name = "FlowNode")
@RestController
@RequestMapping("/FlowNode")
public class FlowNodeController extends EntityController<FlowNodeService, FlowNode, Long> {

}