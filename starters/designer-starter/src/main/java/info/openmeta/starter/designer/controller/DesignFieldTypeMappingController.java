package info.openmeta.starter.designer.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.designer.entity.DesignFieldTypeMapping;
import info.openmeta.starter.designer.service.DesignFieldTypeMappingService;

/**
 * DesignFieldTypeMapping Model Controller
 */
@Tag(name = "DesignFieldTypeMapping")
@RestController
@RequestMapping("/DesignFieldTypeMapping")
public class DesignFieldTypeMappingController extends EntityController<DesignFieldTypeMappingService, DesignFieldTypeMapping, Long> {

}