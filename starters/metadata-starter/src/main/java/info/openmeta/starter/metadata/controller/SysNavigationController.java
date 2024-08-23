package info.openmeta.starter.metadata.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.metadata.entity.SysNavigation;
import info.openmeta.starter.metadata.service.SysNavigationService;

/**
 * SysNavigation Model Controller
 */
@Tag(name = "SysNavigation")
@RestController
@RequestMapping("/SysNavigation")
public class SysNavigationController extends EntityController<SysNavigationService, SysNavigation, Long> {

}