package info.openmeta.starter.metadata.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.metadata.entity.SysField;
import info.openmeta.starter.metadata.service.SysFieldService;

/**
 * SysField Model Controller
 */
@Tag(name = "SysField")
@RestController
@RequestMapping("/SysField")
public class SysFieldController extends EntityController<SysFieldService, SysField, Long> {

}