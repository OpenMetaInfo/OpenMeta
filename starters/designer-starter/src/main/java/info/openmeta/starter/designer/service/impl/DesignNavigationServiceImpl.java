package info.openmeta.starter.designer.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.designer.entity.DesignNavigation;
import info.openmeta.starter.designer.service.DesignNavigationService;

/**
 * DesignNavigation Model Service Implementation
 */
@Service
public class DesignNavigationServiceImpl extends EntityServiceImpl<DesignNavigation, Long> implements DesignNavigationService {

}