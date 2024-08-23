package info.openmeta.starter.metadata.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.metadata.entity.SysApp;
import info.openmeta.starter.metadata.service.SysAppService;

/**
 * SysApp Model Controller
 */
@Tag(name = "SysApp")
@RestController
@RequestMapping("/SysApp")
public class SysAppController extends EntityController<SysAppService, SysApp, Long> {

}