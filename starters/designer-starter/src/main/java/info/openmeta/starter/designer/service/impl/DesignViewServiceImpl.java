package info.openmeta.starter.designer.service.impl;

import org.springframework.stereotype.Service;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.starter.designer.entity.DesignView;
import info.openmeta.starter.designer.service.DesignViewService;

/**
 * DesignView Model Service Implementation
 */
@Service
public class DesignViewServiceImpl extends EntityServiceImpl<DesignView, Long> implements DesignViewService {

}