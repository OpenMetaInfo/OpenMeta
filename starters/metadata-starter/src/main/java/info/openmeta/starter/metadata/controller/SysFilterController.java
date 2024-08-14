package info.openmeta.starter.metadata.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.metadata.entity.SysFilter;
import info.openmeta.starter.metadata.service.SysFilterService;

/**
 * SysFilter Model Controller
 */
@Tag(name = "SysFilter")
@RestController
@RequestMapping("/SysFilter")
public class SysFilterController extends EntityController<SysFilterService, SysFilter, Long> {

}