package info.openmeta.starter.metadata.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.metadata.entity.SysTranslation;
import info.openmeta.starter.metadata.service.SysTranslationService;

/**
 * SysTranslation Model Controller
 */
@Tag(name = "SysTranslation")
@RestController
@RequestMapping("/SysTranslation")
public class SysTranslationController extends EntityController<SysTranslationService, SysTranslation, Long> {

}