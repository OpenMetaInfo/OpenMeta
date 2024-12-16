package info.openmeta.starter.designer.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.designer.entity.DesignModelIndex;
import info.openmeta.starter.designer.service.DesignModelIndexService;

/**
 * DesignModelIndex Model Controller
 */
@Tag(name = "DesignModelIndex")
@RestController
@RequestMapping("/DesignModelIndex")
public class DesignModelIndexController extends EntityController<DesignModelIndexService, DesignModelIndex, Long> {

}