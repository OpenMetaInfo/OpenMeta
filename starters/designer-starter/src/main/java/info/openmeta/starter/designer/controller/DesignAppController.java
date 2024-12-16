package info.openmeta.starter.designer.controller;

import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.designer.entity.DesignApp;
import info.openmeta.starter.designer.service.DesignAppService;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * DesignApp Model Controller
 */
@Tag(name = "DesignApp")
@RestController
@RequestMapping("/DesignApp")
public class DesignAppController extends EntityController<DesignAppService, DesignApp, Long> {

}