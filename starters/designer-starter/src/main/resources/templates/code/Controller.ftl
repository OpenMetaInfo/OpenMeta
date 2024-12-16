package ${packageName}.controller;

import ${packageName}.entity.${modelName};
import ${packageName}.service.${modelName}Service;
import info.openmeta.framework.web.controller.EntityController;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
* ${modelName} model controller
* @author ${userName}
* @date ${currentDate}
*/
@Tag(name = "${modelName}")
@RestController
@RequestMapping("/${modelName}")
public class ${modelName}Controller extends EntityController<${modelName}Service, ${modelName}, Long> {

}