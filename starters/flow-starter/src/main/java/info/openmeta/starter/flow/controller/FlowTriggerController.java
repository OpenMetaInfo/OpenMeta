package info.openmeta.starter.flow.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.flow.entity.FlowTrigger;
import info.openmeta.starter.flow.service.FlowTriggerService;

/**
 * FlowTrigger Model Controller
 */
@Tag(name = "FlowTrigger")
@RestController
@RequestMapping("/FlowTrigger")
public class FlowTriggerController extends EntityController<FlowTriggerService, FlowTrigger, String> {

}