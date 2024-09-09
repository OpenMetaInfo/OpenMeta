package info.openmeta.starter.file.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.file.entity.ExportTemplateField;
import info.openmeta.starter.file.service.ExportTemplateFieldService;

/**
 * ExportTemplateField Model Service Implementation
 */
@Service
public class ExportTemplateFieldServiceImpl extends EntityServiceImpl<ExportTemplateField, Long> implements ExportTemplateFieldService {

}