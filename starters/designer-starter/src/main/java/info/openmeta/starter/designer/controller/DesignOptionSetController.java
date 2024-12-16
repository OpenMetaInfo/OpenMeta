package info.openmeta.starter.designer.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.designer.entity.DesignOptionSet;
import info.openmeta.starter.designer.service.DesignOptionSetService;

/**
 * DesignOptionSet Model Controller
 */
@Tag(name = "DesignOptionSet")
@RestController
@RequestMapping("/DesignOptionSet")
public class DesignOptionSetController extends EntityController<DesignOptionSetService, DesignOptionSet, Long> {

}