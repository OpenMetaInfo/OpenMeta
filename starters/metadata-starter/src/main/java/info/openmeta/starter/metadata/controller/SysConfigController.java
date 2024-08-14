package info.openmeta.starter.metadata.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.metadata.entity.SysConfig;
import info.openmeta.starter.metadata.service.SysConfigService;

/**
 * SysConfig Model Controller
 */
@Tag(name = "SysConfig")
@RestController
@RequestMapping("/SysConfig")
public class SysConfigController extends EntityController<SysConfigService, SysConfig, Long> {

}