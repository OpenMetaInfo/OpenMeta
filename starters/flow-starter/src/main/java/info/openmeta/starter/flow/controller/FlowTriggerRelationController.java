package info.openmeta.starter.flow.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.flow.entity.FlowTriggerRelation;
import info.openmeta.starter.flow.service.FlowTriggerRelationService;

/**
 * FlowTriggerRelation Model Controller
 */
@Tag(name = "FlowTriggerRelation")
@RestController
@RequestMapping("/FlowTriggerRelation")
public class FlowTriggerRelationController extends EntityController<FlowTriggerRelationService, FlowTriggerRelation, Long> {

}