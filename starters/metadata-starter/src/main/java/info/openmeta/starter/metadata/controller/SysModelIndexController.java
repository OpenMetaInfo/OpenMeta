package info.openmeta.starter.metadata.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.metadata.entity.SysModelIndex;
import info.openmeta.starter.metadata.service.SysModelIndexService;

/**
 * SysModelIndex Model Controller
 */
@Tag(name = "SysModelIndex")
@RestController
@RequestMapping("/SysModelIndex")
public class SysModelIndexController extends EntityController<SysModelIndexService, SysModelIndex, Long> {

}