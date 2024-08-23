package info.openmeta.app.demo.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.app.demo.entity.ProjectInfo;
import info.openmeta.app.demo.service.ProjectInfoService;

/**
 * ProjectInfo Model Controller
 */
@Tag(name = "ProjectInfo")
@RestController
@RequestMapping("/ProjectInfo")
public class ProjectInfoController extends EntityController<ProjectInfoService, ProjectInfo, Long> {

}