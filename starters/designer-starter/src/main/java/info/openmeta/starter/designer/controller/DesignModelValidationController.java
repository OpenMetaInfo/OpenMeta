package info.openmeta.starter.designer.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.designer.entity.DesignModelValidation;
import info.openmeta.starter.designer.service.DesignModelValidationService;

/**
 * DesignModelValidation Model Controller
 */
@Tag(name = "DesignModelValidation")
@RestController
@RequestMapping("/DesignModelValidation")
public class DesignModelValidationController extends EntityController<DesignModelValidationService, DesignModelValidation, Long> {

}