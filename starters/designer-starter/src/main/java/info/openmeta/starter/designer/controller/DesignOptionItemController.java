package info.openmeta.starter.designer.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.designer.entity.DesignOptionItem;
import info.openmeta.starter.designer.service.DesignOptionItemService;

/**
 * DesignOptionItem Model Controller
 */
@Tag(name = "DesignOptionItem")
@RestController
@RequestMapping("/DesignOptionItem")
public class DesignOptionItemController extends EntityController<DesignOptionItemService, DesignOptionItem, Long> {

}