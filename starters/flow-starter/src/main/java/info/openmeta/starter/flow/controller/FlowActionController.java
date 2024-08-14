package info.openmeta.starter.flow.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.flow.entity.FlowAction;
import info.openmeta.starter.flow.service.FlowActionService;

/**
 * FlowAction Model Controller
 */
@Tag(name = "FlowAction")
@RestController
@RequestMapping("/FlowAction")
public class FlowActionController extends EntityController<FlowActionService, FlowAction, Long> {

}