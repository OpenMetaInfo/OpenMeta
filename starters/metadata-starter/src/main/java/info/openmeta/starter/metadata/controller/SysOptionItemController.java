package info.openmeta.starter.metadata.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.metadata.entity.SysOptionItem;
import info.openmeta.starter.metadata.service.SysOptionItemService;

/**
 * SysOptionItem Model Controller
 */
@Tag(name = "SysOptionItem")
@RestController
@RequestMapping("/SysOptionItem")
public class SysOptionItemController extends EntityController<SysOptionItemService, SysOptionItem, Long> {

}