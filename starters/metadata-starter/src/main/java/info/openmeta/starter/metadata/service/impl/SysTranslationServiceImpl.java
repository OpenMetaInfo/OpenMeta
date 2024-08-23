package info.openmeta.starter.metadata.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.metadata.entity.SysTranslation;
import info.openmeta.starter.metadata.service.SysTranslationService;

/**
 * SysTranslation Model Service Implementation
 */
@Service
public class SysTranslationServiceImpl extends EntityServiceImpl<SysTranslation, Long> implements SysTranslationService {

}