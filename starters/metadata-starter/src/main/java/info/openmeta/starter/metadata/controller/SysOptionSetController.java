package info.openmeta.starter.metadata.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.metadata.entity.SysOptionSet;
import info.openmeta.starter.metadata.service.SysOptionSetService;

/**
 * SysOptionSet Model Controller
 */
@Tag(name = "SysOptionSet")
@RestController
@RequestMapping("/SysOptionSet")
public class SysOptionSetController extends EntityController<SysOptionSetService, SysOptionSet, Long> {

}