package info.openmeta.starter.designer.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.designer.entity.DesignModelValidation;
import info.openmeta.starter.designer.service.DesignModelValidationService;

/**
 * DesignModelValidation Model Service Implementation
 */
@Service
public class DesignModelValidationServiceImpl extends EntityServiceImpl<DesignModelValidation, Long> implements DesignModelValidationService {

}