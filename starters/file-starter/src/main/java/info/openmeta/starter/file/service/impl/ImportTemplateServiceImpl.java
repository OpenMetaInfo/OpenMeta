package info.openmeta.starter.file.service.impl;

import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.file.entity.ImportTemplate;
import info.openmeta.starter.file.service.ImportTemplateService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * ImportTemplateService implementation
 */
@Slf4j
@Service
public class ImportTemplateServiceImpl extends EntityServiceImpl<ImportTemplate, Long> implements ImportTemplateService {

}