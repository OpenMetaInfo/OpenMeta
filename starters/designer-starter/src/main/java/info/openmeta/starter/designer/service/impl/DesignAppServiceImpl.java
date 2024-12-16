package info.openmeta.starter.designer.service.impl;

import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.designer.entity.DesignApp;
import info.openmeta.starter.designer.service.DesignAppService;
import org.springframework.stereotype.Service;

/**
 * DesignApp Model Service Implementation
 */
@Service
public class DesignAppServiceImpl extends EntityServiceImpl<DesignApp, Long> implements DesignAppService {

}