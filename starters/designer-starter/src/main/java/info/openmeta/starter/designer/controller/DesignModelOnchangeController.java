package info.openmeta.starter.designer.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.designer.entity.DesignModelOnchange;
import info.openmeta.starter.designer.service.DesignModelOnchangeService;

/**
 * DesignModelOnchange Model Controller
 */
@Tag(name = "DesignModelOnchange")
@RestController
@RequestMapping("/DesignModelOnchange")
public class DesignModelOnchangeController extends EntityController<DesignModelOnchangeService, DesignModelOnchange, Long> {

}