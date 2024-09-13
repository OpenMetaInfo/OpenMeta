package info.openmeta.starter.file.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.file.entity.ImportTemplateField;
import info.openmeta.starter.file.service.ImportTemplateFieldService;

/**
 * ImportTemplateField Model Service Implementation
 */
@Service
public class ImportTemplateFieldServiceImpl extends EntityServiceImpl<ImportTemplateField, Long> implements ImportTemplateFieldService {

}