package info.openmeta.starter.designer.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.designer.entity.DesignFieldTypeMapping;
import info.openmeta.starter.designer.service.DesignFieldTypeMappingService;

/**
 * DesignFieldTypeMapping Model Service Implementation
 */
@Service
public class DesignFieldTypeMappingServiceImpl extends EntityServiceImpl<DesignFieldTypeMapping, Long> implements DesignFieldTypeMappingService {

}