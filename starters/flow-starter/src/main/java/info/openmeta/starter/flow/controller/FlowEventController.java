package info.openmeta.starter.flow.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.flow.entity.FlowEvent;
import info.openmeta.starter.flow.service.FlowEventService;

/**
 * FlowEvent Model Controller
 */
@Tag(name = "FlowEvent")
@RestController
@RequestMapping("/FlowEvent")
public class FlowEventController extends EntityController<FlowEventService, FlowEvent, Long> {

}