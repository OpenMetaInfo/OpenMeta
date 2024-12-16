package info.openmeta.starter.designer.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.designer.entity.DesignConfig;
import info.openmeta.starter.designer.service.DesignConfigService;

/**
 * DesignConfig Model Controller
 */
@Tag(name = "DesignConfig")
@RestController
@RequestMapping("/DesignConfig")
public class DesignConfigController extends EntityController<DesignConfigService, DesignConfig, Long> {

}