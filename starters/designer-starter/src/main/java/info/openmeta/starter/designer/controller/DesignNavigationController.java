package info.openmeta.starter.designer.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.designer.entity.DesignNavigation;
import info.openmeta.starter.designer.service.DesignNavigationService;

/**
 * DesignNavigation Model Controller
 */
@Tag(name = "DesignNavigation")
@RestController
@RequestMapping("/DesignNavigation")
public class DesignNavigationController extends EntityController<DesignNavigationService, DesignNavigation, Long> {

}