package info.openmeta.starter.file.service.impl;

import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.file.entity.ExportTemplate;
import info.openmeta.starter.file.service.ExportTemplateService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Export template service implementation class
 */
@Service
@Slf4j
public class ExportTemplateServiceImpl extends EntityServiceImpl<ExportTemplate, Long> implements ExportTemplateService {

}