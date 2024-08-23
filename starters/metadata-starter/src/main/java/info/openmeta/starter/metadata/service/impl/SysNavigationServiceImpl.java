package info.openmeta.starter.metadata.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.metadata.entity.SysNavigation;
import info.openmeta.starter.metadata.service.SysNavigationService;

/**
 * SysNavigation Model Service Implementation
 */
@Service
public class SysNavigationServiceImpl extends EntityServiceImpl<SysNavigation, Long> implements SysNavigationService {

}