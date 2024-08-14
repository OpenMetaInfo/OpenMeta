package info.openmeta.starter.flow.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.flow.entity.FlowRoute;
import info.openmeta.starter.flow.service.FlowRouteService;

/**
 * FlowRoute Model Controller
 */
@Tag(name = "FlowRoute")
@RestController
@RequestMapping("/FlowRoute")
public class FlowRouteController extends EntityController<FlowRouteService, FlowRoute, Long> {

}