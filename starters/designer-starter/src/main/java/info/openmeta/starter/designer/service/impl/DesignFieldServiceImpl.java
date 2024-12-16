package info.openmeta.starter.designer.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.designer.entity.DesignField;
import info.openmeta.starter.designer.service.DesignFieldService;

/**
 * DesignField Model Service Implementation
 */
@Service
public class DesignFieldServiceImpl extends EntityServiceImpl<DesignField, Long> implements DesignFieldService {

}