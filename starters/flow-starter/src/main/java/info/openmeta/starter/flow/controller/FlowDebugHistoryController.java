package info.openmeta.starter.flow.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.flow.entity.FlowDebugHistory;
import info.openmeta.starter.flow.service.FlowDebugHistoryService;

/**
 * FlowDebugHistory Model Controller
 */
@Tag(name = "FlowDebugHistory")
@RestController
@RequestMapping("/FlowDebugHistory")
public class FlowDebugHistoryController extends EntityController<FlowDebugHistoryService, FlowDebugHistory, Long> {

}