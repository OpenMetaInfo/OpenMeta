package info.openmeta.starter.designer.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.designer.entity.DesignView;
import info.openmeta.starter.designer.service.DesignViewService;

/**
 * DesignView Model Controller
 */
@Tag(name = "DesignView")
@RestController
@RequestMapping("/DesignView")
public class DesignViewController extends EntityController<DesignViewService, DesignView, Long> {

}