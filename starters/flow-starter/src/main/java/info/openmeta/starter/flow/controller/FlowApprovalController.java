package info.openmeta.starter.flow.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.flow.entity.FlowApproval;
import info.openmeta.starter.flow.service.FlowApprovalService;

/**
 * FlowApproval Model Controller
 */
@Tag(name = "FlowApproval")
@RestController
@RequestMapping("/FlowApproval")
public class FlowApprovalController extends EntityController<FlowApprovalService, FlowApproval, Long> {

}