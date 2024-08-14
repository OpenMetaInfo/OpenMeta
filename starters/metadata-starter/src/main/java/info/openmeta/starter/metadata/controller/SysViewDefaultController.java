package info.openmeta.starter.metadata.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.metadata.entity.SysViewDefault;
import info.openmeta.starter.metadata.service.SysViewDefaultService;

/**
 * SysViewDefault Model Controller
 */
@Tag(name = "SysViewDefault")
@RestController
@RequestMapping("/SysViewDefault")
public class SysViewDefaultController extends EntityController<SysViewDefaultService, SysViewDefault, Long> {

}