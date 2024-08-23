package info.openmeta.app.demo.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.app.demo.entity.EmpInfo;
import info.openmeta.app.demo.service.EmpInfoService;

/**
 * EmpInfo Model Controller
 */
@Tag(name = "EmpInfo")
@RestController
@RequestMapping("/EmpInfo")
public class EmpInfoController extends EntityController<EmpInfoService, EmpInfo, Long> {

}