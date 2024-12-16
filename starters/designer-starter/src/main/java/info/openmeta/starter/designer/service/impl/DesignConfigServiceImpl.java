package info.openmeta.starter.designer.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.designer.entity.DesignConfig;
import info.openmeta.starter.designer.service.DesignConfigService;

/**
 * DesignConfig Model Service Implementation
 */
@Service
public class DesignConfigServiceImpl extends EntityServiceImpl<DesignConfig, Long> implements DesignConfigService {

}